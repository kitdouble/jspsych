var likert_scale = [

 "Very Inaccurate", 
        "Moderately Inaccurate", 
        "Neither Accurate Nor Inaccurate", 
        "Moderately Accurate", 
        "Very Accurate"
];


var questions1 = {
  type: jsPsychSurveyLikert,
  scale_width: 500,
  preamble: '<h3>Instructions</h3><p style = "text-align: left;">Describe yourself as you generally are now, not as you wish to be in the future. Describe yourself as you honestly see yourself, in relation to other people you know of the same sex as you are, and roughly your same age. So that you can describe yourself in an honest manner, your responses will be kept in absolute confidence.<br>Indicate for each statement whether it is 1. Very Inaccurate, 2. Moderately Inaccurate, 3. Neither Accurate Nor Inaccurate, 4. Moderately Accurate, or 5. Very Accurate as a description of you.</p>',
  questions: [
{prompt: 'Am the life of the party.', name: 'Q1', labels: likert_scale, required: true},
{prompt: 'Feel little concern for others.', name: 'Q2', labels: likert_scale, required: true},
{prompt: 'Am always prepared.', name: 'Q3', labels: likert_scale, required: true},
{prompt: 'Get stressed out easily.', name: 'Q4', labels: likert_scale, required: true},
{prompt: 'Have a rich vocabulary.', name: 'Q5', labels: likert_scale, required: true},
{prompt: 'Don&apos;t talk a lot.', name: 'Q6', labels: likert_scale, required: true},
{prompt: 'Am interested in people.', name: 'Q7', labels: likert_scale, required: true},
{prompt: 'Leave my belongings around.', name: 'Q8', labels: likert_scale, required: true},
{prompt: 'Am relaxed most of the time.', name: 'Q9', labels: likert_scale, required: true},
{prompt: 'Have difficulty understanding abstract ideas.', name: 'Q10', labels: likert_scale, required: true},
  ],
  randomize_question_order: false,
     on_finish: function(data){
      data.trial_type = "Survey";

    },
};




var questions2 = {
  type: jsPsychSurveyLikert,
  scale_width: 500,
  preamble: '<h3>Instructions</h3><p style = "text-align: left;">Describe yourself as you generally are now, not as you wish to be in the future. Describe yourself as you honestly see yourself, in relation to other people you know of the same sex as you are, and roughly your same age. So that you can describe yourself in an honest manner, your responses will be kept in absolute confidence.<br>Indicate for each statement whether it is 1. Very Inaccurate, 2. Moderately Inaccurate, 3. Neither Accurate Nor Inaccurate, 4. Moderately Accurate, or 5. Very Accurate as a description of you.</p>',
  questions: [
{prompt: 'Feel comfortable around people.', name: 'Q11', labels: likert_scale, required: true},
{prompt: 'Insult people.', name: 'Q12', labels: likert_scale, required: true},
{prompt: 'Pay attention to details.', name: 'Q13', labels: likert_scale, required: true},
{prompt: 'Worry about things.', name: 'Q14', labels: likert_scale, required: true},
{prompt: 'Have a vivid imagination.', name: 'Q15', labels: likert_scale, required: true},
{prompt: 'Keep in the background.', name: 'Q16', labels: likert_scale, required: true},
{prompt: 'Sympathize with others&apos; feelings.', name: 'Q17', labels: likert_scale, required: true},
{prompt: 'Make a mess of things.', name: 'Q18', labels: likert_scale, required: true},
{prompt: 'Seldom feel blue.', name: 'Q19', labels: likert_scale, required: true},
{prompt: 'Am not interested in abstract ideas.', name: 'Q20', labels: likert_scale, required: true}
  ],
  randomize_question_order: false,
      on_finish: function(data){
      data.trial_type = "Survey";

    },
};



var questions3 = {
  type: jsPsychSurveyLikert,
  scale_width: 500,
  preamble: '<h3>Instructions</h3><p style = "text-align: left;">Describe yourself as you generally are now, not as you wish to be in the future. Describe yourself as you honestly see yourself, in relation to other people you know of the same sex as you are, and roughly your same age. So that you can describe yourself in an honest manner, your responses will be kept in absolute confidence.<br>Indicate for each statement whether it is 1. Very Inaccurate, 2. Moderately Inaccurate, 3. Neither Accurate Nor Inaccurate, 4. Moderately Accurate, or 5. Very Accurate as a description of you.</p>',
  questions: [
{prompt: 'Start conversations.', name: 'Q21', labels: likert_scale, required: true},
{prompt: 'Am not interested in other people&apos;s problems.', name: 'Q22', labels: likert_scale, required: true},
{prompt: 'Get chores done right away.', name: 'Q23', labels: likert_scale, required: true},
{prompt: 'Am easily disturbed.', name: 'Q24', labels: likert_scale, required: true},
{prompt: 'Have excellent ideas.', name: 'Q25', labels: likert_scale, required: true},
{prompt: 'Have little to say.', name: 'Q26', labels: likert_scale, required: true},
{prompt: 'Have a soft heart.', name: 'Q27', labels: likert_scale, required: true},
{prompt: 'Often forget to put things back in their proper place.', name: 'Q28', labels: likert_scale, required: true},
{prompt: 'Get upset easily.', name: 'Q29', labels: likert_scale, required: true},
{prompt: 'Do not have a good imagination.', name: 'Q30', labels: likert_scale, required: true},
  ],
  randomize_question_order: false,
      on_finish: function(data){
      data.trial_type = "Survey";

    },
};




var questions4 = {
  type: jsPsychSurveyLikert,
  scale_width: 500,
  preamble: '<h3>Instructions</h3><p style = "text-align: left;">Describe yourself as you generally are now, not as you wish to be in the future. Describe yourself as you honestly see yourself, in relation to other people you know of the same sex as you are, and roughly your same age. So that you can describe yourself in an honest manner, your responses will be kept in absolute confidence.<br>Indicate for each statement whether it is 1. Very Inaccurate, 2. Moderately Inaccurate, 3. Neither Accurate Nor Inaccurate, 4. Moderately Accurate, or 5. Very Accurate as a description of you.</p>',
  questions: [
{prompt: 'Talk to a lot of different people at parties.', name: 'Q31', labels: likert_scale, required: true},
{prompt: 'Am not really interested in others.', name: 'Q32', labels: likert_scale, required: true},
{prompt: 'Like order.', name: 'Q33', labels: likert_scale, required: true},
{prompt: 'Change my mood a lot.', name: 'Q34', labels: likert_scale, required: true},
{prompt: 'Am quick to understand things.', name: 'Q35', labels: likert_scale, required: true},
{prompt: 'Don&apos;t like to draw attention to myself.', name: 'Q36', labels: likert_scale, required: true},
{prompt: 'Take time out for others.', name: 'Q37', labels: likert_scale, required: true},
{prompt: 'Shirk my duties.', name: 'Q38', labels: likert_scale, required: true},
{prompt: 'Have frequent mood swings.', name: 'Q39', labels: likert_scale, required: true},
{prompt: 'Use difficult words.', name: 'Q40', labels: likert_scale, required: true},
  ],
  randomize_question_order: false,
      on_finish: function(data){
      data.trial_type = "Survey";

    },
};




var questions5 = {
  type: jsPsychSurveyLikert,
  scale_width: 500,
  preamble: '<h3>Instructions</h3><p style = "text-align: left;">Describe yourself as you generally are now, not as you wish to be in the future. Describe yourself as you honestly see yourself, in relation to other people you know of the same sex as you are, and roughly your same age. So that you can describe yourself in an honest manner, your responses will be kept in absolute confidence.<br>Indicate for each statement whether it is 1. Very Inaccurate, 2. Moderately Inaccurate, 3. Neither Accurate Nor Inaccurate, 4. Moderately Accurate, or 5. Very Accurate as a description of you.</p>',
  questions: [
{prompt: 'Don&apos;t mind being the center of attention.', name: 'Q41', labels: likert_scale, required: true},
{prompt: 'Feel others&apos; emotions.', name: 'Q42', labels: likert_scale, required: true},
{prompt: 'Follow a schedule.', name: 'Q43', labels: likert_scale, required: true},
{prompt: 'Get irritated easily.', name: 'Q44', labels: likert_scale, required: true},
{prompt: 'Spend time reflecting on things.', name: 'Q45', labels: likert_scale, required: true},
{prompt: 'Am quiet around strangers.', name: 'Q46', labels: likert_scale, required: true},
{prompt: 'Make people feel at ease.', name: 'Q47', labels: likert_scale, required: true},
{prompt: 'Am exacting in my work.', name: 'Q48', labels: likert_scale, required: true},
{prompt: 'Often feel blue.', name: 'Q49', labels: likert_scale, required: true},
{prompt: 'Am full of ideas.', name: 'Q50', labels: likert_scale, required: true},
  ],
  randomize_question_order: false,
      on_finish: function(data){
      data.trial_type = "Survey";

    },
};


var question_inst = {
  type: jsPsychInstructions,
  pages: ['<h3>Welcome!</h3>Before commencing the main task, you will need to complete the following questionnaire'],
  show_clickable_nav: true
}






var score_survey = {
    type: jsPsychCallFunction,
    func: function(data){

    },
    on_finish: function(data){


      var survey1 = jsPsych.data.get().filter({trial_type: "Survey"}).last(5).values()[0].response;
     var survey2 = jsPsych.data.get().filter({trial_type: "Survey"}).last(5).values()[1].response;
     var survey3 = jsPsych.data.get().filter({trial_type: "Survey"}).last(5).values()[2].response;
     var survey4 = jsPsych.data.get().filter({trial_type: "Survey"}).last(5).values()[3].response;
     var survey5 = jsPsych.data.get().filter({trial_type: "Survey"}).last(5).values()[4].response;



      data.Q1 = survey1.Q1;
      data.Q2 = survey1.Q2;
      data.Q3 = survey1.Q3;
      data.Q4 = survey1.Q4;
      data.Q5 = survey1.Q5;
      data.Q6 = survey1.Q6;
      data.Q7 = survey1.Q7;
      data.Q8 = survey1.Q8;
      data.Q9 = survey1.Q9;
      data.Q10 = survey1.Q10;


      data.Q11 = survey2.Q11;
      data.Q12 = survey2.Q12;
      data.Q13 = survey2.Q13;
      data.Q14 = survey2.Q14;
      data.Q15 = survey2.Q15;
      data.Q16 = survey2.Q16;
      data.Q17 = survey2.Q17;
      data.Q18 = survey2.Q18;
      data.Q19 = survey2.Q19;
      data.Q20 = survey2.Q20;


      data.Q21 = survey3.Q21;
      data.Q22 = survey3.Q22;
      data.Q23 = survey3.Q23;
      data.Q24 = survey3.Q24;
      data.Q25 = survey3.Q25;
      data.Q26 = survey3.Q26;
      data.Q27 = survey3.Q27;
      data.Q28 = survey3.Q28;
      data.Q29 = survey3.Q29;
      data.Q30 = survey3.Q30;

      data.Q31 = survey4.Q31;
      data.Q32 = survey4.Q32;
      data.Q33 = survey4.Q33;
      data.Q34 = survey4.Q34;
      data.Q35 = survey4.Q35;
      data.Q36 = survey4.Q36;
      data.Q37 = survey4.Q37;
      data.Q38 = survey4.Q38;
      data.Q39 = survey4.Q39;
      data.Q40 = survey4.Q40;

      data.Q41 = survey5.Q41;
      data.Q42 = survey5.Q42;
      data.Q43 = survey5.Q43;
      data.Q44 = survey5.Q44;
      data.Q45 = survey5.Q45;
      data.Q46 = survey5.Q46;
      data.Q47 = survey5.Q47;
      data.Q48 = survey5.Q48;
      data.Q49 = survey5.Q49;
      data.Q50 = survey5.Q50;


      data.trial_type = "Survey Data"



      data.Extra =  (data.Q1) + (6 - data.Q6)+ (data.Q11)+ (6 - data.Q16)+ (data.Q21)+ (6 - data.Q26)+ (data.Q31)+ (6 - data.Q36)+ (data.Q41)+ (6 - data.Q46);
      data.Agree = (6 - data.Q2)+ (data.Q7)+ (6 - data.Q12)+ (data.Q17)+ (6 - data.Q22)+ (data.Q27)+ (6 - data.Q32)+ (data.Q37)+ (data.Q42)+ (data.Q47);
      data.Consc = (data.Q3)+ (6 - data.Q8)+ (data.Q13)+ (6 - data.Q18)+ (data.Q23)+ (6 - data.Q28)+ (data.Q33)+ (6 - data.Q38)+ (data.Q43)+ (data.Q48);
      data.EmoStab = (6 - data.Q4)+ (data.Q9)+ (6 - data.Q14)+ (data.Q19)+ (6 - data.Q24)+ (6 - data.Q29)+ (6 - data.Q34)+ (6 - data.Q39)+ (6 - data.Q44)+ (6 - data.Q49);
      data.Open = (data.Q5)+ (6 - data.Q10)+ (data.Q15)+ (6 - data.Q20)+ (data.Q25)+ (6 - data.Q30)+ (data.Q35)+ (data.Q40)+ (data.Q45)+ (data.Q50);

    }
}







var questions = {
  timeline: [question_inst, questions1, questions2, questions3, questions4, questions5, score_survey]
}

