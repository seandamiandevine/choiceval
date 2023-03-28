var lCount      = 0; 
var tCount      = 0; 
var mCount      = 0; 
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
    return `<img src="static/images/cards/${f1}.svg" width="${CARD_SIZE[0]}px" height="${CARD_SIZE[1]}px"></img>`;

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
    mi_type     = DECKMAPPING[chosen_deck][1];
    
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

var feedback = {
  type: 'html-keyboard-response',
  stimulus: function() {

    if(timedout) return '<p style="font-size:50px">Too slow!</p>'; ; 

    if(chosen_deck.includes('X')) {
      // Catch deck
      outcome = 1; 
    } else if(DECKMAPPING[chosen_deck][1]==0) {
      // MI = 0
      if(phase=='choice') {
        outcome = Math.random() < 0.5 ? 1:0;
      } else {
        outcome = LEARNING_OUT_LIST[lCount]; 
      }
    } else {
      // MI = 1
      outcome = win_card==1 ? 1:0; 
    };

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

var mock_slider = {
  type: 'html-slider-response', 
  stimulus: `<p>How confident are you that you will win ${POINTS} points?</p>`,
  require_movement: true, 
  labels: ['0%', '50%', '100%'], 
  slider_width: 500, 
  on_finish(data) {
    mock_rating = data.response; 
  }
}

var sense_of_control_slider = {
  type: 'html-slider-response', 
  stimulus: function() {

    col  = DECKMAPPING[DECKS[socCount]][2]; 
    sym  = DECKMAPPING[DECKS[socCount]][3];

    f    = `stack_of_${col}_${sym}`; 
    html = '<p>When you chose this deck, how much did you feel like you could control whether you earned points or not?</p>';
    html += `<img src="static/images/cards/${f}.svg" width="${CARD_SIZE[0]-100}px" height="${CARD_SIZE[1]-100}px"></img>`;

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
    data.trial         = phase=='choice' ? tCount : lCount; 
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
    data.mock_rating   = mock_rating; 
  }
};

var end_screen = {
  type: 'html-keyboard-response',
  stimulus: 'Thank you for your participation! You are done the study. Your completion code is C1CI1SVL. Copy this code <b>now</b>.\nThen, press SPACE and this page should redirect to Prolific shortly. If it does not, paste the code you copied into your Prolific app.',
  choices: ['space'],
};


