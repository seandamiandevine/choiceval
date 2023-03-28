var PROL_ID = jsPsych.data.getURLVariable('PROLIFIC_PID');
var CTBL = _.sample([0,1,2,3]); 
jsPsych.data.addProperties({
  subject: Math.random().toString(36).slice(2), 
  date: Date(),
  prol_id: PROL_ID,
  ctbl: CTBL
});

const DEBUGMODE         = false;                                                 // whether to skip instructions + practice (for debugging) 
const DECKS             = ['A','B','C','D'];                                     // A: C/MI=1, B:NC/MI=1, C:C/MI=0, D:NC/MI=0;
const POINTS            = 10;                                                    // points for winning
const CHOICE_OPTS       = ['c','nc'];                                            // possible choice conditions
const MI_OPTS           = [0,1];                                                 // possible Mutual info conditions
const VALUES            = [1,2,3,4,6,7,8,9,10];                                  // card values (no 5)
const IMGDIR            = 'static/images/'                                       // directory where stim are stored
const CARD_KEYS         = ['z', 'm'];                                            // keys to choose cards (learning phase)
const DECK_KEYS         = ['w', 'o'];                                            // keys to choose decks (choice phase)
const CARD_SIZE         = [175, 250];                                            // [w,h] of cards (in px.)
const DECKDISPTIME      = 5000;                                                  // time that doors stay up (in ms.)
const MAXCARDTIME       = 3000;                                                  // response deadline for choosing card in learning phase (in ms.)
const CARDDISPTIME      = 1500;                                                  // time that selected card is shown in learning phase (in ms.)
const FEEDBACKTIME      = 1000;                                                  // time that points are shown in learning phase (in ms.)
const ITI               = 500;                                                   // iti (in ms.)
const NUM_LEARN         = DEBUGMODE ? 2:4;                                       // number of learning reps
const NUM_CHOICE_REPS   = DEBUGMODE ? 1:10;                                      // number of choice reps
const NUM_CATCH_REPS    = 1; 
const NUM_MOCK_REPS     = 1; 
const CTBLMAPPING       = {0: [['red','black'],['stars','moons']], //[choice,nochoice], [mi=1,mi=0]
                           1: [['red','black'],['moons','stars']], 
                           2: [['black','red'],['stars','moons']], 
                           3: [['black','red'],['moons','stars']]};
const DECKMAPPING       = {'A'  :['c' , 1, CTBLMAPPING[CTBL][0][0], CTBLMAPPING[CTBL][1][0]],
                           'B'  :['nc', 1, CTBLMAPPING[CTBL][0][1], CTBLMAPPING[CTBL][1][0]], 
                           'C'  :['c' , 0, CTBLMAPPING[CTBL][0][0], CTBLMAPPING[CTBL][1][1]], 
                           'D'  :['nc', 0, CTBLMAPPING[CTBL][0][1], CTBLMAPPING[CTBL][1][1]], 
                           'Xc' :['c',  0, 'red',   'bolts'], 
                           'Xnc':['nc', 0, 'black', 'bolts']} 

// make sure each deck wins and loses an equal amount of times
// and make sure MI=0 decks provide equivalent feedback
const LEARNING_LIST   = Array(NUM_LEARN).fill(DECKS).flat();         
var   LEARNING_WIN_LIST = Array(LEARNING_LIST.length);
var   LEARNING_OUT_LIST = Array(LEARNING_LIST.length);

for(let d = 0; d < DECKS.length; d++) {
  tmp1 = _.shuffle(Array(Math.round(NUM_LEARN/2)).fill([0,1]).flat()); 
  tmp2 = _.shuffle(Array(Math.round(NUM_LEARN/2)).fill([0,1]).flat()); 
  counter = 0;
  for(let i = 0; i < LEARNING_LIST.length; i++) {
    if(LEARNING_LIST[i]==DECKS[d]) {
      LEARNING_WIN_LIST[i] = tmp1[counter]; 
      LEARNING_OUT_LIST[i] = tmp2[counter]==1 ? Math.abs(LEARNING_WIN_LIST[i]-1) : LEARNING_WIN_LIST[i]; 
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
}

// generate  balanced pairs
var CHOICE_LISTS = [];
for (let r = 0; r < NUM_CHOICE_REPS; r++) {
  for(let i = 0; i < DECKS.length; i++) {
    for(let j = 0; j < DECKS.length; j++) {
      if(i >= j) continue; // avoid repeats
      CHOICE_LISTS.push([DECKS[i], DECKS[j]]);
    }
  }
};
  
// add one catch per deck
for (let r = 0; r < NUM_CATCH_REPS; r++) {
  for(let i = 0; i < DECKS.length; i++) {
    CHOICE_LISTS.push([DECKS[i], 'Xc'], [DECKS[i], 'Xnc']); 
  }
}; 

CHOICE_LISTS = _.shuffle(CHOICE_LISTS);
var WIN_LIST = _.shuffle(Array(CHOICE_LISTS.length/2).fill([0,1]).flat());


// make mock trial list
var MOCK_LIST = Array(NUM_MOCK_REPS).fill(DECKS).flat(); 

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

  choice1 = `<img src="static/images/cards/${choices[0]}.svg" width="${cue_size[0]}px" height="${cue_size[1]}px"></img>`;
  choice2 = `<img src="static/images/cards/${choices[1]}.svg" width="${cue_size[0]}px" height="${cue_size[1]}px"></img>`;
  key1    = `<img src="static/images/keys/${keys[0]}.svg" width="35%" height="75%"></img>`;
  key2    = `<img src="static/images/keys/${keys[1]}.svg" width="35%" height="75%"></img>`;

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



