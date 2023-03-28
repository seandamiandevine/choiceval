
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

var init_mock_phase = {
  type:'call-function', 
  func: function() {
    phase = 'mock';
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
      save_to_db();
      return false; 
    }
    lCount ++; 
    timedout = false; 
    return true; 
  }, 
};

var choice_phase = {
  timeline: [show_deck_options, show_card_options, show_result, feedback, save_choice_data], 
  loop_function: function() {
    if(tCount>=(CHOICE_LISTS.length-1)) {
      tCount    = 0; 
      save_to_db();
      return false; 
    }
    tCount ++; 
    timedout = false; 
    return true; 
  }, 
};

var mock_phase = {
  timeline: [show_deck_cue, show_card_options, show_result, mock_slider, save_choice_data], 
  loop_function: function() {
    if(mCount>=(MOCK_LIST.length-1)) {
      mCount      = 0; 
      deck1       = null; 
      chosen_deck = null; 
      chosen_col  = null; 
      chosen_sym  = null; 
      save_to_db();
      return false; 
    }
    mCount ++; 
    timedout = false; 
    return true; 
  }, 
};

var SoC_phase = {
  timeline: [sense_of_control_slider], 
  loop_function: function() {
    if(socCount>=(DECKS.length-1)) {
      save_to_db();
      return false; 
    }
    socCount ++; 
    return true; 
  }, 
};





