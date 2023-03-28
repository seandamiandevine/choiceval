// Miscelaneous preperation stuff
 jsPsych.data.addProperties({
   subject: Math.random().toString(36).slice(2), 
   date: Date.now()
 });

var fs = {
	type: 'fullscreen', 
  fullscreen_mode: true,
  on_start: function(){
    // set up task appearence
    document.body.style.background = "white";
    document.body.style.color = 'black'   
  }
};

img_preload = []; 

for(let s=0; s<3; s++) {
    shape = ['moons', 'stars', 'bolts'][s];
  for(let c=0; c<2; c++) {
      col = ['black', 'red'][c]; 
      img_preload.push(`static/images/cards/back_of_${col}_${shape}.svg`); 
      img_preload.push(`static/images/cards/stack_of_${col}_${shape}.svg`); 
    for(let i=1; i<10; i++) {
      img_preload.push(`static/images/cards/${i}_of_${col}_${shape}.svg`); 
    }
  }
}; 

for (let cb=0; cb<4; cb++){
  for(let i=1; i<27; i++) {
    img_preload.push(`static/images/instructions_CB_${cb}/Slide${i}.png`); 
  }
}

// Setup Timeline
if(DEBUGMODE) {
  timeline = [fs, init_learning_phase, instructions, learning_phase, init_choice_phase, instructions, choice_phase, instructions, init_mock_phase, mock_phase, task2Q, demographics, end_screen];
} else {
  timeline = [fs, consent, init_learning_phase, instructions, learning_phase, init_choice_phase, instructions, choice_phase, instructions, init_mock_phase, mock_phase, instructions, SoC_phase, task2Q, demographics, end_screen];
}

save_to_db = function() {
  $.ajax({
    type: "POST",
    url: "/save_data",
    data: jsPsych.data.get().json(),
    contentType: "application/json",
    dataType: 'json',
    success: function(result) {
      if(result) {
        console.log(result);
      }
    }
  })
};

// Run and preload images
jsPsych.init({
    timeline: timeline,
		show_preload_progress_bar: true,
    preload_images: img_preload,  
    on_finish: function() {
      if(DEBUGMODE) {
        jsPsych.data.get().localSave('csv', '_debug.csv');
        return null;
      }

      Promise.all([save_to_db()]).then(() => {
        save_to_db(); 
        window.open('https://app.prolific.co/submissions/complete?cc=C1CI1SVL', '_blank');
      }).catch(() => {
        // resubmit
        save_to_db();
        window.open('https://app.prolific.co/submissions/complete?cc=C1CI1SVL', '_blank');
      })

      // jsPsych.data.get().localSave('csv', 'undergrad_pilot_local.csv'); // only for piloting

    }
});
