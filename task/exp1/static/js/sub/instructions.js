var INST_RATIO       = 1.77;
var INST_HEIGHT      = window.innerHeight;
var INST_WIDTH       = INST_RATIO*INST_HEIGHT;
var INSTIDX          = 1; 
var INST_SECTIONS    = [19, 24, 25, 26];
var INST_SECTION_IDX = 0; 

var inst_screen = {
    type: 'image-keyboard-response',
    stimulus: function() {
      return `static/images/instructions_CB_${CTBL}/Slide${INSTIDX}.png`
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


