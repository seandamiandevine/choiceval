// ****************************************************************************
// *                                 Constants                                *
// ****************************************************************************

var PROL_ID = jsPsych.data.getURLVariable('PROLIFIC_PID');
var CTBL    = 1//_.sample([0,1]); 

jsPsych.data.addProperties({
  subject: Math.random().toString(36).slice(2), 
  date: Date(),
  prol_id: PROL_ID,
  ctbl: CTBL
});

const DEBUGMODE         = false;                    // whether to skip instructions + practice (for debugging) 
const DECKS             = ['A','B'];                // A: Choice; B: No choice
const POINTS            = 10;                       // points for winning
const CHOICE_OPTS       = ['c','nc'];               // possible choice conditions
const MI_OPTS           = [0,1];                    // possible Mutual info conditions
const VALUES            = [1,2,3,4,6,7,8,9,10];     // card values (no 5)
const IMGDIR            = 'stim/'                   // directory where stim are stored
const CARD_KEYS         = ['z', 'm'];               // keys to choose cards (learning phase)
const DECK_KEYS         = ['w', 'o'];               // keys to choose decks (choice phase)
const CARD_SIZE         = [175, 250];               // [w,h] of cards (in px.)
const DECKDISPTIME      = 5000;                     // time that doors stay up (in ms.)
const MAXCARDTIME       = 3000;                     // response deadline for choosing card in learning phase (in ms.)
const CARDDISPTIME      = 1500;                     // time that selected card is shown in learning phase (in ms.)
const FEEDBACKTIME      = 1000;                     // time that points are shown in learning phase (in ms.)
const ITI               = 500;                      // iti (in ms.)
const NUM_LEARN         = DEBUGMODE ? 2:4;          // number of learning reps
const NUM_TRIALS        = DEBUGMODE ? 10:54;        // number of trials per block (including catch decks)
const NUM_CHOICE_REPS   = DEBUGMODE ? 2:4;          // number of choice blocks
const NUM_CATCH_REPS    = 1;                        // number of catch trials per block
const NUM_MOCK_REPS     = DEBUGMODE ? 2:10;          // number of mock trials per blocks


// cb: 0
// const DECKMAPPING       = {'A'  :['c' , 1, 'red'  , 'stars'],
//                            'B'  :['nc', 1, 'black', 'stars'], 
//                            'Xc' :['c' , 0, 'red'  , 'bolts'], 
//                            'Xnc':['nc', 0, 'black', 'bolts']
//                          } 

// cb: 1
const DECKMAPPING       = {'A'  :['c' , 1, 'black' , 'stars'],
                           'B'  :['nc', 1, 'red'  , 'stars'], 
                           'Xc' :['c' , 0, 'black', 'bolts'], 
                           'Xnc':['nc', 0, 'red'  , 'bolts']
                         } 

// make sure each deck wins and loses an equal amount of times
// and make sure MI=0 decks provide equivalent feedback
const LEARNING_LIST     = Array(NUM_LEARN).fill(DECKS).flat();         
var   LEARNING_WIN_LIST = Array(LEARNING_LIST.length);
var   LEARNING_OUT_LIST = Array(LEARNING_LIST.length);

for(let d = 0; d < DECKS.length; d++) {
  tmp1 = _.shuffle(Array(Math.round(NUM_LEARN/2)).fill([0,1]).flat()); 
  tmp2 = _.shuffle(Array(Math.round(NUM_LEARN/2)).fill([0,1]).flat()); 
  counter = 0;
  for(let i = 0; i < LEARNING_LIST.length; i++) {
    if(LEARNING_LIST[i]==DECKS[d]) {

      LEARNING_WIN_LIST[i] = tmp1[counter]; 
      if(counter < NUM_LEARN/2) {
        // learn MI = 1
        LEARNING_OUT_LIST[i] = tmp1[counter];
      } else {
        // learn MI = 0
        LEARNING_OUT_LIST[i] = tmp1[counter]==1 ? 0:1;
      }

      counter += 1;  
    }
  }
};

// add one catch per learning phase rep
counter = 0; 
tmp1    = _.shuffle(Array(Math.round(NUM_LEARN/2)).fill([0,1]).flat()); 
tmp2    = _.shuffle(Array(Math.round(NUM_LEARN/2)).fill([0,1]).flat()); 
for(let i=0; i<LEARNING_LIST.length; i++) {
  if(LEARNING_LIST[i]==DECKS[DECKS.length-1]){
    LEARNING_LIST.splice(i+1,0,'Xc','Xnc');
    LEARNING_WIN_LIST.splice(i+1,0,tmp1[counter],tmp2[counter]);
    LEARNING_OUT_LIST.splice(i+1,0,1,1);
    counter +=1; 
  }
};


// win_count = {"A":0,'B':0,'Xc':0,'Xnc':0};
// out_count = {"A":0,'B':0,'Xc':0,'Xnc':0};
// for(let i = 0; i < LEARNING_LIST.length; i++) {
  
//   win_count[LEARNING_LIST[i]] += LEARNING_WIN_LIST[i];
//   out_count[LEARNING_LIST[i]] += LEARNING_OUT_LIST[i];

// }
// console.log(win_count);
// console.log(out_count);


// generate  balanced pairs
var CHOICE_LISTS = [];
for (let r = 0; r < NUM_CHOICE_REPS; r++) {
    CHOICE_LISTS.push([DECKS[0], 'Xc'], [DECKS[0], 'Xnc']); 
    CHOICE_LISTS.push([DECKS[1], 'Xc'], [DECKS[1], 'Xnc']); 
  for(let i = 0; i < NUM_TRIALS-4; i++) {
      CHOICE_LISTS.push([DECKS[0], DECKS[1]]);
    }
  };
  

CHOICE_LISTS = _.shuffle(CHOICE_LISTS);
var WIN_LIST = _.shuffle(Array(CHOICE_LISTS.length/2).fill([0,1]).flat());


// Xcount = {'AXc':0,'BXc':0,'AXnc':0,'BXnc':0}
// for(let i = 0; i < CHOICE_LISTS.length; i++) {

//     if(CHOICE_LISTS[i][1].includes('X')) {
//       Xcount[CHOICE_LISTS[i][0] + CHOICE_LISTS[i][1]] ++; 
//     } 
// }

// generate MI list
if(CTBL==0) {
  var MI_LIST = Array(NUM_CHOICE_REPS/2).fill(MI_OPTS).flat(); // this could also be shuffled
} else {
  var MI_LIST = Array(NUM_CHOICE_REPS/2).fill(MI_OPTS.reverse()).flat(); // this could also be shuffled
}



// make mock trial list
var MOCK_LIST = Array(NUM_CHOICE_REPS)
for(let r = 0; r < NUM_CHOICE_REPS; r++) {
  idx = _.shuffle(_.range(Math.round(NUM_TRIALS*.1),NUM_TRIALS));  // must complete at least 10% of trials before making judgement
  idx = idx.splice(0,NUM_MOCK_REPS)
  MOCK_LIST[r] = idx
}

function ChoiceHTML(choices, keys=DECK_KEYS, hide=null, cue_size=CARD_SIZE, scale=300) {
  /**
 * Generates HTML for choice trials, which are two decks side-by-side with a key to press beneath. 
 * @param  {[Array]}    choices    The effort levels to display (length must be 2)
 * @param  {[Array]}    keys       The keys to display (defaults to DECK_KEYS in constants.js)
 * @param  {[Integer]}  hide       Index of key to be hidden for no-choice trials
 * @param  {[Array]}    cue_size   Size of the pie charts (defaults to CARD_SIZE from constants.js)
 * @param  {[Integer]}  font_size  The size of the point text (defaults to FONTSIZE in constants.js)
 * @param  {[Integer]}  scale      Scaling factor between cues (defaults to 2*CUESIZE[0])
 * 
 * @return {[Array||String]}       HTML elements for each component
 */

  if(choices.length != keys.length) {
    throw('choices and points must be equal size!')
  }

  if(choices.length > 2) {
    throw ('provide only two choices.');
  }

  choice1 = `<img src="stim/cards/${choices[0]}.svg" width="${cue_size[0]}px" height="${cue_size[1]}px"></img>`;
  choice2 = `<img src="stim/cards/${choices[1]}.svg" width="${cue_size[0]}px" height="${cue_size[1]}px"></img>`;
  key1    = `<img src="stim/keys/${keys[0]}.svg" width="35%" height="75%"></img>`;
  key2    = `<img src="stim/keys/${keys[1]}.svg" width="35%" height="75%"></img>`;

  if (hide==0) key1 = ''; 
  if (hide==1) key2 = ''; 


  html = `
  <table>
  <tr height=${scale}>
    <td width=${scale} height=${scale}>${choice1}</td>
    <td width=${scale} height=${scale}>${choice2}</td>
  </tr>
  <tr height=${scale-100}>
    <td width=${scale}>${key1}</td>
    <td width=${scale}>${key2}</td>
  </tr>
</table>
  `;

  return html; 

};

MI2P = function(x,min=0,max=1,a=0.5,b=1) {
  /**
 * Converts Mutual Information index into a bounded probability [a,b]
 */
  
  return (b-a)*(x-min/(max-min))+a;
  
}

// ****************************************************************************
// *                               Instructions                               *
// ****************************************************************************

var INST_RATIO       = 1.77;
var INST_HEIGHT      = window.innerHeight;
var INST_WIDTH       = INST_RATIO*INST_HEIGHT;
var INSTIDX          = 1; 
var INST_SECTIONS    = [17, 23, 24];
var INST_SECTION_IDX = 0; 

var inst_screen = {
    type: 'image-keyboard-response',
    stimulus: function() {
      return `stim/instructions_CB_${CTBL}/Slide${INSTIDX}.png`
    },
    choices: ['leftarrow', 'rightarrow'],
    stimulus_height: INST_HEIGHT,
    stimulus_width: INST_WIDTH, 
    on_finish: function(data) {
        key_press = jsPsych.pluginAPI.convertKeyCodeToKeyCharacter(data.key_press);
        if(key_press=='rightarrow') { 
            INSTIDX++; 
        } else {
            INSTIDX = Math.max(1,INSTIDX-1); 
        }
    }
};

var instructions = {
  timeline: [inst_screen], 
  loop_function: function() {
    if(INSTIDX>INST_SECTIONS[INST_SECTION_IDX]) {
        INST_SECTION_IDX += 1; 
        return false; 
    }
    return true; 
  }, 
}; 


// ****************************************************************************
// *                                  Trials                                  *
// ****************************************************************************

var lCount      = 0; 
var bCount      = 0; 
var tTrial      = 0; 
var tCount      = 0; 
var socCount    = 0; 
var phase       = null; 
var timedout    = null; 
var deck1       = null; 
var col1        = null; 
var sym1        = null; 
var deck2       = null;
var col2        = null; 
var sym2        = null; 
var pair        = [null,null];
var chosen_deck = null; 
var chosen_col  = null; 
var chosen_sym  = null;
var deck_rt     = null; 
var choice_type = null; 
var mi_type     = null; 
var hidden_key  = null;
var chosen_card = null;
var card_rt     = null;
var player_num  = null;
var win_card    = null; 
var outcome     = null; 
var is_mock     = null; 
var mock_rating = null; 

var show_deck_options = {
  type: 'html-keyboard-response',
  stimulus: function() {
    deck1 = CHOICE_LISTS[tCount][0];
    deck2 = CHOICE_LISTS[tCount][1];

    if(Math.random() < .5) {
      pair  = [deck1, deck2]; 
    } else {
      pair = [deck2, deck1];
    }

    col1 = DECKMAPPING[pair[0]][2]; 
    sym1 = DECKMAPPING[pair[0]][3];
    col2 = DECKMAPPING[pair[1]][2];
    sym2 = DECKMAPPING[pair[1]][3];
    f1 = `stack_of_${col1}_${sym1}`; 
    f2 = `stack_of_${col2}_${sym2}`; 

    return ChoiceHTML([f1,f2], keys=DECK_KEYS);
  },
  choices: DECK_KEYS,
  trial_duration: DECKDISPTIME,
  post_trial_gap: ITI, 
  on_finish: function(data){

    if(data.key_press==null) {
      timedout     = true;
      key_press    = null;
      deck_rt      = null; 
      chosen_deck  = null;
      chosen_col   = null; 
      chosen_sym   = null; 
    } else {
      key_press   = jsPsych.pluginAPI.convertKeyCodeToKeyCharacter(data.key_press);
      deck_rt     = data.rt;
      chosen_deck = key_press==DECK_KEYS[0] ? pair[0]:pair[1]; 
      chosen_col  = DECKMAPPING[chosen_deck][2];
      chosen_sym  = DECKMAPPING[chosen_deck][3];
    }
  }
};

var show_deck_cue = {
  type: 'html-keyboard-response',
  stimulus: function() {
    if(phase=='learning') {
      chosen_deck = deck1 = LEARNING_LIST[lCount]; 
    } else {
      chosen_deck = deck1 = MOCK_LIST[mCount]; 
    }

    chosen_col = col1 = DECKMAPPING[deck1][2]; 
    chosen_sym = sym1 = DECKMAPPING[deck1][3];
    f1         = `stack_of_${col1}_${sym1}`; 
    return `<img src="stim/cards/${f1}.svg" width="${CARD_SIZE[0]}px" height="${CARD_SIZE[1]}px"></img>`;

  },
  choices: DECK_KEYS,
  trial_duration: CARDDISPTIME,
  post_trial_gap: ITI
};

var show_card_options = {
  type: 'html-keyboard-response',
  stimulus: function() {
    if(timedout) return ''; 

    choice_type = DECKMAPPING[chosen_deck][0];
    mi_type     = MI_LIST[bCount]; //DECKMAPPING[chosen_deck][1];
    
    if(choice_type=='nc') {
      hidden_key = Math.random() < .5 ? 0:1;
    } else {
      hidden_key = 'NA'; 
    }
    f1 = `back_of_${chosen_col}_${chosen_sym}`; 
    return ChoiceHTML([f1, f1], keys=CARD_KEYS, hide=hidden_key);
  },
  choices: function() {
    if(choice_type=='c') return CARD_KEYS; 
    return CARD_KEYS[Math.abs(hidden_key-1)]; 

  },
  trial_duration: function() {
    if(timedout) return 0 
    if(phase=='mock') return null
    return MAXCARDTIME
  },
  on_finish: function(data){
    if(data.key_press==null) {
      timedout    = true;
      key_press   = null;
      card_rt     = null;
      chosen_card = null; 
    } else {
      key_press   = jsPsych.pluginAPI.convertKeyCodeToKeyCharacter(data.key_press);
      card_rt     = data.rt;
      chosen_card = key_press==CARD_KEYS[0] ? 'left':'right'; 
    }
  }
};

var show_result = {
  type: 'html-keyboard-response',
  stimulus: function() {

    if(timedout) return ''; 

    if(phase=='mock') {
      win_card = 1; 
    } else {
      win_card = phase=='choice' ? WIN_LIST[tCount] : LEARNING_WIN_LIST[lCount]; 
    }
    if(win_card==1) {
      player_num = _.sample(VALUES.filter(i => i>5));
    } else {
      player_num = _.sample(VALUES.filter(i => i<5));
    }

    front = `${player_num}_of_${chosen_col}_${chosen_sym}`;
    back  = `back_of_${chosen_col}_${chosen_sym}`; 
    result = chosen_card=='left' ? [front, back] : [back, front];

    return ChoiceHTML(result, keys=CARD_KEYS, hide=hidden_key);
  },
  choices: jsPsych.NO_KEYS,
  trial_duration: function() {
    if(timedout) return 0;
    return CARDDISPTIME; 
  },
  post_trial_gap: ITI
  };

var mock_slider = {
  type: 'html-slider-response', 
  stimulus: function() {

    if(timedout) return ''; 

    if(MOCK_LIST[bCount].includes(tCount)) {
      is_mock = true; 
      return `<p>How confident are you that you will win ${POINTS} points?</p>`
      }
      is_mock = false; 
      return '';
    },
  require_movement: true, 
  labels: ['Not all confident in winning', 'Very confident in winning'], 
  slider_width: 500, 
  trial_duration: function() {

    if(timedout) return 0; 
    if(MOCK_LIST[bCount].includes(tCount)) return null; 
    return 0;
  },
  on_finish(data) {
    mock_rating = data.response; 
  }
}

var feedback = {
  type: 'html-keyboard-response',
  stimulus: function() {

    if(timedout) return '<p style="font-size:50px">Too slow!</p>'; ; 

    if(chosen_deck.includes('X')) {
      // Catch deck (always win)
      outcome = 1; 
    } else if(phase=='learning'){
      outcome = LEARNING_OUT_LIST[lCount]; 
    } else {
      // Probability of win based on MI (weighted coin flip) and whether win_card==1
      if(win_card==1) {
        outcome = Math.random() < MI2P(mi_type) ? 1:0;
      } else {
        outcome = Math.random() < MI2P(mi_type) ? 0:1;
      }
    }

    if(outcome==1) {
      return `<p style="font-size:50px">You win!\n+${POINTS} points this round!</p>`; 
    } else {
      return '<p style="font-size:50px">You Lose!\nNo points this round.</p>';
    }
  },
  choices: jsPsych.NO_KEYS,
  trial_duration: FEEDBACKTIME,
  post_trial_gap: ITI
};

var sense_of_control_slider = {
  type: 'html-slider-response', 
  stimulus: function() {

    col  = DECKMAPPING[DECKS[socCount]][2]; 
    sym  = DECKMAPPING[DECKS[socCount]][3];

    f    = `stack_of_${col}_${sym}`; 
    html = '<p>When you chose this deck, how much did you feel like you could control whether you earned points or not?</p>';
    html += `<img src="stim/cards/${f}.svg" width="${CARD_SIZE[0]-100}px" height="${CARD_SIZE[1]-100}px"></img>`;

    return html;

  },
  require_movement: true, 
  labels: ['No Control', 'Intermediate Control', 'Complete Control'], 
  slider_width: 500, 
  post_trial_gap: ITI, 
  on_finish(data) {
    tmp = {};
    tmp[`sense_of_control_${DECKS[socCount]}`] = data.response; 
    jsPsych.data.addProperties(tmp);
  }
};


save_choice_data = {
  type: 'html-keyboard-response',
  stimulus:'', 
  trial_duration: 0, 
  on_finish: function(data) {
    data.is_data_trial = true; 
    data.phase         = phase; 
    data.trial         = phase=='choice' ? tTrial : 'NA';
    data.trialinblock  = phase=='choice' ? tCount : lCount; 
    data.deck1         = deck1; 
    data.deck2         = deck2; 
    data.col1          = col1; 
    data.col2          = col2; 
    data.sym1          = sym1; 
    data.sym2          = sym2; 
    data.deck_l        = pair[0];
    data.deck_r        = pair[1]; 
    data.chosen_deck   = chosen_deck; 
    data.chosen_col    = chosen_col; 
    data.chosen_sym    = chosen_sym; 
    data.deck_rt       = deck_rt; 
    data.choose_choice = choice_type=='c' ? 1:0;
    data.mutual_info   = mi_type;
    data.hidden_key    = hidden_key; 
    data.chosen_card   = chosen_card; 
    data.card_rt       = card_rt; 
    data.player_num    = player_num; 
    data.win_card      = win_card; 
    data.outcome       = outcome;  
    data.is_mock       = is_mock; 
    data.mock_rating   = mock_rating; 
  }
};

var end_block = {
  type: 'instructions',
  pages: [
    '<p>Your day at the casino is done!</p>'+
    '<p>Press NEXT to start the next day.</p>' +
    '<img src="stim/moon.gif" width="100">'
  ],
  show_clickable_nav: true,
  post_trial_gap: ITI
};

var end_screen = {
  type: 'html-keyboard-response',
  stimulus: 'Thank you for your participation! You are done the study. Your completion code is C1CI1SVL. Copy this code <b>now</b>.\nThen, press SPACE and this page should redirect to Prolific shortly. If it does not, paste the code you copied into your Prolific app.',
  choices: ['space'],
};


// ****************************************************************************
// *                                   Loops                                  *
// ****************************************************************************


var init_learning_phase = {
  type:'call-function', 
  func: function() {
    phase = 'learning';
  }
};

var init_choice_phase = {
  type:'call-function', 
  func: function() {
    phase = 'choice';
  }
};

var learning_phase = {
  timeline: [show_deck_cue, show_card_options, show_result, feedback, save_choice_data], 
  loop_function: function() {
    if(lCount>=(LEARNING_LIST.length-1)) {
      lCount      = 0; 
      deck1       = null; 
      chosen_deck = null; 
      chosen_col  = null; 
      chosen_sym  = null; 
      return false; 
    }
    lCount ++; 
    timedout = false; 
    return true; 
  }, 
};

var choice_block = {
  timeline: [show_deck_options, show_card_options, show_result, mock_slider, feedback, save_choice_data], 
  loop_function: function() {
    if(tCount>=(NUM_TRIALS-1)) return false; 
    tCount ++; 
    tTrial ++; 
    timedout = false; 
    return true; 
  }, 
};

var choice_phase = {
  timeline: [choice_block, end_block], 
  loop_function: function() {
    if(bCount >= (NUM_CHOICE_REPS-1)) return false;
    tCount  = 0; 
    timedout = false; 
    bCount ++; 
    return true; 
  }, 
}

var SoC_phase = {
  timeline: [sense_of_control_slider], 
  loop_function: function() {
    if(socCount>=(DECKS.length-1)) {
      return false; 
    }
    socCount ++; 
    return true; 
  }, 
};

// ****************************************************************************
// *                                 Questions                                *
// ****************************************************************************

var task2Q = {
    type: 'html-keyboard-response',
    stimulus: function() {
        html =  '<p>You are finished the main part of the study!</p>';
        html += '<p>We just have a few questions for you to answer and then you will be done for today. Please answer these questions honestly.</p>';
        html += '<p>Press SPACE to continue.</p>';
        return html
    },
    choices: ['space'], 
    on_finish: function() {
      document.body.style.background = "white";
      document.body.style.color = 'black'   
    }
};

var demographics = {
  type: 'survey-text',
  questions: [
    {prompt: "For statistical purposes, how old are you? (please type out in years)", name:'age', required: true},
    {prompt: "For statistical purposes, What is your gender? (please type out your answer below)", name:'gender', required: true}
      ],
  preamble: 'Please answer these questions.',
  on_finish: function(data) {
    jsPsych.data.addProperties({
      age:    JSON.parse(data.responses)['age'],
      gender: JSON.parse(data.responses)['gender']
    });

  }
};

var debrief_questions = {
  type: 'survey-text',
  questions: [
    {prompt: "Did you have any explicit strategy while completing the task?", name:'strategy', required: true},
    {prompt: "Do you have any other comments you would like to share?", name:'comments', required: true},
      ],
  preamble: 'Please answer these questions.',
  on_finish: function(data) {
    jsPsych.data.addProperties({
      strategy: JSON.parse(data.responses)['strategy'],
      comments: JSON.parse(data.responses)['comments']
    });
  }
};


var DASS21 = {
  type: 'survey-likert',
  questions: [
    {prompt: "I found it hard to wind down", name:'DASS1', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I was aware of dryness of my mouth", name:'DASS2', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I couldn’t seem to experience any positive feeling at all", name:'DASS3', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I experienced breathing difficulty (e.g. excessively rapid breathing, breathlessness in the absence of physical exertion)", name:'DASS4', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I found it difficult to work up the initiative to do things", name:'DASS5', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I tended to over-react to situations", name:'DASS6', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I experienced trembling (e.g. in the hands)", name:'DASS7', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I felt that I was using a lot of nervous energy", name:'DASS8', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I was worried about situations in which I might panic and make a fool of myself", name:'DASS9', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I felt that I had nothing to look forward to I found myself getting agitated", name:'DASS10', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I found myself getting agitated", name:'DASS11', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I found it difficult to relax", name:'DASS12', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I felt down-hearted and blue", name:'DASS13', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I was intolerant of anything that kept me from getting on with what I was doing", name:'DASS14', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I felt I was close to panic", name:'DASS15', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I was unable to become enthusiastic about anything'", name:'DASS16', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I felt I wasn’t worth much as a person", name:'DASS17', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I felt that I was rather touchy", name:'DASS18', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I was aware of the action of my heart in the absence of physical exertion (e.g. sense of heart rate increase, heart missing a beat)", name:'DASS19', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I felt scared without any good reason.", name:'DASS20', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']},
    {prompt: "I felt that life was meaningless", name:'DASS21', required: true, labels:['Did not apply to me much or very often','1','2','Applied to me very much or most of the time']}

      ],
  preamble: 'Please answer these questions. The rating scale is as follows: 0 =Did not apply to me at all, 1 = Applied to me to some degree, or some of the time, 2 =Applied to me to a considerable degree or a good part of time, 3 =Applied to me very much or most of the time',
  scale_width: 500, 
  on_finish: function(data) {
    jsPsych.data.addProperties({
      DASS1_s:     JSON.parse(data.responses)['DASS1'],
      DASS2_a:     JSON.parse(data.responses)['DASS2'],
      DASS3_d:     JSON.parse(data.responses)['DASS3'],
      DASS4_a:     JSON.parse(data.responses)['DASS4'],
      DASS5_d:     JSON.parse(data.responses)['DASS5'],
      DASS6_s:     JSON.parse(data.responses)['DASS6'],
      DASS7_a:     JSON.parse(data.responses)['DASS7'],
      DASS8_s:     JSON.parse(data.responses)['DASS8'],
      DASS9_a:     JSON.parse(data.responses)['DASS9'],
      DASS10_d:    JSON.parse(data.responses)['DASS10'],
      DASS11_s:    JSON.parse(data.responses)['DASS11'],
      DASS12_s:    JSON.parse(data.responses)['DASS12'],
      DASS13_d:    JSON.parse(data.responses)['DASS13'],
      DASS14_s:    JSON.parse(data.responses)['DASS14'],
      DASS15_a:    JSON.parse(data.responses)['DASS15'],
      DASS16_d:    JSON.parse(data.responses)['DASS16'],
      DASS17_d:    JSON.parse(data.responses)['DASS17'],
      DASS18_s:    JSON.parse(data.responses)['DASS18'], 
      DASS19_a:    JSON.parse(data.responses)['DASS19'],
      DASS20_a:    JSON.parse(data.responses)['DASS20'],
      DASS21_d:    JSON.parse(data.responses)['DASS21']
    });
  }
};

var BISBAS = {
  // see https://local.psy.miami.edu/people/faculty/ccarver/availbale-self-report-instruments/bisbas-scales/
  type: 'survey-likert',
  questions: [
    {prompt: "A person's family is the most important thing in life.", name:'BB1', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "Even if something bad is about to happen to me, I rarely experience fear or nervousness.", name:'BB2', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I go out of my way to get things I want.", name:'BB3', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "When I'm doing well at something I love to keep at it.", name:'BB4', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I'm always willing to try something new if I think it will be fun.", name:'BB5', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "How I dress is important to me.", name:'BB6', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "When I get something I want, I feel excited and energized.", name:'BB7', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "Criticism or scolding hurts me quite a bit.", name:'BB8', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "When I want something I usually go all-out to get it.", name:'BB9', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I will often do things for no other reason than that they might be fun.", name:'BB10', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "It's hard for me to find the time to do things such as get a haircut.", name:'BB11', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "If I see a chance to get something I want I move on it right away.", name:'BB12', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I feel pretty worried or upset when I think or know somebody is angry at me.", name:'BB13', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "When I see an opportunity for something I like I get excited right away.", name:'BB14', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I often act on the spur of the moment.", name:'BB15', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "If I think something unpleasant is going to happen I usually get pretty worked up.", name:'BB16', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I often wonder why people act the way they do.", name:'BB17', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "When good things happen to me, it affects me strongly.", name:'BB18', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I feel worried when I think I have done poorly at something important.", name:'BB19', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I crave excitement and new sensations.", name:'BB20', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "When I go after something I use a no holds barred approach.", name:'BB21', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I have very few fears compared to my friends.", name:'BB22', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "It would excite me to win a contest.", name:'BB23', required: true, labels:['Very true for me','2','3','Very false for me']},
    {prompt: "I worry about making mistakes.", name:'BB24', required: true, labels:['Very true for me','2','3','Very false for me']}
      ],
  preamble: 'Please answer these questions. The rating scale is as follows:\n1 = Very true for me, 2 = Somewhat true for me, 3 = Somewhat false for me, 4 = Very false for me.',
  scale_width: 500, 
  on_finish: function(data) {
    jsPsych.data.addProperties({
      BB1:     JSON.parse(data.responses)['BB1'],
      BB2:     JSON.parse(data.responses)['BB2'],
      BB3:     JSON.parse(data.responses)['BB3'],
      BB4:     JSON.parse(data.responses)['BB4'],
      BB5:     JSON.parse(data.responses)['BB5'],
      BB6:     JSON.parse(data.responses)['BB6'],
      BB7:     JSON.parse(data.responses)['BB7'],
      BB8:     JSON.parse(data.responses)['BB8'],
      BB9:     JSON.parse(data.responses)['BB9'],
      BB10:    JSON.parse(data.responses)['BB10'],
      BB11:    JSON.parse(data.responses)['BB11'],
      BB12:    JSON.parse(data.responses)['BB12'],
      BB13:    JSON.parse(data.responses)['BB13'],
      BB14:    JSON.parse(data.responses)['BB14'],
      BB15:    JSON.parse(data.responses)['BB15'],
      BB16:    JSON.parse(data.responses)['BB16'],
      BB17:    JSON.parse(data.responses)['BB17'],
      BB18:    JSON.parse(data.responses)['BB18'], 
      BB19:    JSON.parse(data.responses)['BB19'],
      BB20:    JSON.parse(data.responses)['BB20'],
      BB21:    JSON.parse(data.responses)['BB21'],
      BB22:    JSON.parse(data.responses)['BB22'],
      BB23:    JSON.parse(data.responses)['BB23'],
      BB24:    JSON.parse(data.responses)['BB24']
    });
  }
};

var NFC = {
  // see https://centerofinquiry.org/uncategorized/need-for-cognition-scale-wabash-national-study/
  type: 'survey-likert',
  questions: [
    {prompt: "I would prefer complex to simple problems.", name:'NFC1', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I like to have the responsibility of handling a situation that requires a lot of thinking.", name:'NFC2', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "Thinking is not my idea of fun.", name:'NFC3', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I would rather do something that requires little thought than something that is sure to challenge my thinking abilities.", name:'NFC4', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I try to anticipate and avoid situations where there is likely a chance I will have to think in depth about something.", name:'NFC5', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I find satisfaction in deliberating hard and for long hours.", name:'NFC6', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I would prefer complex to simple problems.", name:'NFC1', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I only think as hard as I have to.", name:'NFC7', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I prefer to think about small, daily projects to long-term ones.", name:'NFC8', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I like tasks that require little thought once I’ve learned them.", name:'NFC9', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "The idea of relying on thought to make my way to the top appeals to me.", name:'NFC10', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I really enjoy a task that involves coming up with new solutions to problems.", name:'NFC11', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "Learning new ways to think doesn’t excite me very much.", name:'NFC12', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I prefer my life to be filled with puzzles that I must solve.", name:'NFC13', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "The notion of thinking abstractly is appealing to me.", name:'NFC14', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought.", name:'NFC15', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I feel relief rather than satisfaction after completing a task that required a lot of mental effort.", name:'NFC16', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "It’s enough for me that something gets the job done; I don’t care how or why it works.", name:'NFC17', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']},
    {prompt: "I usually end up deliberating about issues even when they do not affect me personally.", name:'NFC18', required: true, labels:['Extremely uncharacteristic','2','3','4','Extremely characteristic']}
      ],
  preamble: 'For each of the statements below, please indicate to what extent the statement is characteristic of you. The rating scale is as follows: 1 = extremely uncharacteristic, 2 = somewhat uncharacteristic, 3 = uncertain, 4 = somewhat characteristc, 5 = extremely characteristic',
  scale_width: 500, 
  on_finish: function(data) {
    jsPsych.data.addProperties({
      NFC1:     JSON.parse(data.responses)['NFC1'],
      NFC2:     JSON.parse(data.responses)['NFC2'],
      NFC3:     JSON.parse(data.responses)['NFC3'],
      NFC4:     JSON.parse(data.responses)['NFC4'],
      NFC5:     JSON.parse(data.responses)['NFC5'],
      NFC6:     JSON.parse(data.responses)['NFC6'],
      NFC7:     JSON.parse(data.responses)['NFC7'],
      NFC8:     JSON.parse(data.responses)['NFC8'],
      NFC9:     JSON.parse(data.responses)['NFC9'],
      NFC10:    JSON.parse(data.responses)['NFC10'],
      NFC11:    JSON.parse(data.responses)['NFC11'],
      NFC12:    JSON.parse(data.responses)['NFC12'],
      NFC13:    JSON.parse(data.responses)['NFC13'],
      NFC14:    JSON.parse(data.responses)['NFC14'],
      NFC15:    JSON.parse(data.responses)['NFC15'],
      NFC16:    JSON.parse(data.responses)['NFC16'],
      NFC17:    JSON.parse(data.responses)['NFC17'],
      NFC18:    JSON.parse(data.responses)['NFC18']
    });
  }
};


// ****************************************************************************
// *                                    Run                                   *
// ****************************************************************************

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

for(let s=0; s<2; s++) {
    shape = ['stars', 'bolts'][s];
  for(let c=0; c<2; c++) {
      col = ['black', 'red'][c]; 
      img_preload.push(`stim/cards/back_of_${col}_${shape}.svg`); 
      img_preload.push(`stim/cards/stack_of_${col}_${shape}.svg`); 
    for(let i=1; i<10; i++) {
      img_preload.push(`stim/cards/${i}_of_${col}_${shape}.svg`); 
    }
  }
}; 

for(let i=1; i<26; i++) {
    img_preload.push(`stim/instructions_CB_0/Slide${i}.png`); 
  }

// Setup Timeline
if(DEBUGMODE) {
  timeline = [fs, init_learning_phase, instructions, learning_phase, init_choice_phase, instructions, choice_phase, instructions, task2Q, demographics, end_screen];
} else {
  timeline = [fs, init_learning_phase, instructions, learning_phase, init_choice_phase, instructions, choice_phase, instructions, SoC_phase, task2Q, demographics, end_screen];
}

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
stimwindow.open('https://app.prolific.co/submissions/complete?cc=C1CI1SVL', '_blank');
    }
});
