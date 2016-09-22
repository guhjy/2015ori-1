# Response to reviewers ORIIR160019-01-00

We thank the reviewer for the time taken to look at and think about our proposal. Below, we respond point-by-point to the different weaknesses mentioned by the reviewer.

# Reviewer remark 1
The application does not provide clear evidence that the type of misconduct to which scientists admit is aligned with the kind of misconduct that the proposed methods would be able to address.

## Response

We never aimed to support such a claim; we only aimed to sketch the problem with the way estimates of data fabrication are now constructed. We fully agree with the reviewer that there is no way of knowing whether the data fabrication admitted to is the same as the data fabrication these methods could detect.

# Reviewer remark 2

The application did not explain why it only focuses on social and medical sciences.

## Response

We focus only on the social- and medical sciences given the widespread use of randomized experimental procedures in these fields and our familiarity with the misconduct cases in these fields. We could expand to other fields with sufficient resources, but given the constraints of the current grant scheme we focused on these fields.

# Reviewer remark 3

The Principal Investigator does not have experience overseeing a large project given that he is still emerging as a scientist in a Ph.D. program.

## Response

The PI will be supervised during this process by the PhD supervisors, who cumulatively have managed projects amounting to approximately 4 million euro's.

# Reviewer remark 4

There is no investigator with qualitative analysis experience.

## Response

The research team is embedded in a methods and statistics department, with experts on qualitative analysis who will be queried, if necessary, during the project.

# Reviewer remark 5

Many literatures have suggested the possibility of using automatic tools to screen for data anomalies, so the
idea proposed is not innovative.

## Response

We did not intend to propose this to be our innovation --- we only wanted to implement the idea after repeated suggestions.

# Reviewer remark 6

There are some weaknesses in Project 1, which include: 1) The application does not consider the situation where researchers do not fabricate the entire data but only modify some numbers in real data, 2) Researchers can improve the way they fabricate data if they know what kinds of statistical methods are used for fabrication detection, 3) Twenty participants might not cover all or most of fabrication behaviors, and 4) There is a possibility that none of the methods can distinguish genuine data from fabricated data. Therefore, it is unclear how the applicant will continue to Project 2 and 3.

## Response

Weakness 1: The reviewer correctly identifies that the projects only look at fabrication, and not falsification. This is a limitation and we look to improve upon this in the Phase II grant proposal. 

Weakness 2: Detecting data fabrication will become a cat-and-mouse game indeed, as the reviewer points out. However, there is always the higher-dimensional aspect. If a researcher fabricates data that cannot be detected as such at the univariate level, detecting data fabrication can be done at all the higher levels still (bivariate, then trivariate, etc.). At some point the fabrication process will become so complicated in order to not be detected, that it is more effortful to fabricate data than to actually conduct a study. 

Weakness 3: Twenty participants is most likely not as variable as we would want to have our results. Note that this number is the result of a feasibility analysis based on the contents of the project, the strain on the participants, and the resources available. 

Weakness 4: Project 2 and 3 can continue regardless of the success of methods used in Project 1, given that if the methods fail Project 2 is more important (how did they fabricate and is it reasonable that the results did not pick this up + what new methods would have picked it up?). Project 3 is infrastructure development and will not actually apply the methods from project 1 because it does not extract the raw data, so it is independent of the results of project 1 (the infrastructure is used to extract information for analysis, the analysis method is not fixed). Moreover, we have inspected similar methods based on summary results that proved to be effective. Hence, we are confident that Project 1 will be successful.

# Reviewer remark 7

The application does not make a convincing case that the limited timeframe available to study participants to construct fabricated data is relevant to contexts where actual fabrication takes place.

## Response

Given the remark by the reviewer, we discussed this issue and adjusted our protocol to allow the time spent fabricating the data to be at the discretion of the participant. They will be invited to participate and, if they accept, they will be prompted to finish fabricating data before the on site interview. This timeframe will be several weeks instead of 45 minutes.

# Reviewer remark 8

This application does not clearly demonstrate an inductive methodology.

## Response

With the ROC analyses we apply an inductive methodology to assess the weight of the premise "the statistical test for data fabrication is significant" in support of the conclusion that the data were in fact fabricated. For project 2 and 3, we indeed do not demonstrate an inductive methodology because Project 2 applies abductive reasoning and project 3 uses statistical testing as an inductive methodology, which was validated in project 1 (and other work). 

# Reviewer remark 9

The application does not provide a convincing basis for drawing sharp inferences regarding the types of fabrication that present the greatest risk given the modest number of available participants.

## Response

We are unsure of what the reviewer means with this comment, given that part of the proposal is to discover more about how data is fabricated. It is difficult to state which type of data fabrication poses the greatest risk if we do not know what types there are.

# Reviewer remark 10

In Project 3, the investigators plan to use existing software for publication screening before applying their proposed detection method. This screening step might be risky since this software may fail to identify certain types of data anomalies.

## Response

The reviewer correctly remarks that certain types of data anomalies might be missed. If missed, it does not get "cleared" so to speak: the flagging system only aims to identify potential problems, not to say that those that aren't flagged do not contain problems. 

# Reviewer remark 11

It is not clear in the application why material flagged by ContentMine would necessarily be thought of as presenting fabrication risk, which suggests a substantial proportion of false positives.

## Response

No material will be flagged in relation to the subproject with ContentMine, we will only develop software and validate its performance in extracting information (not analyzing or qualifying those data).

# Reviewer remark 12

There are no details on the specific resources available to study staff in completion of the proposed research, such as computer capability, storage, security, and software programs. Similarly, there should be separate information about Content Mine, as they are a central resource for the development of proposed Phase 3 tools.

## Response

Tilburg University provides the research team with sufficient office space, computer facilities and support. Software programs used will all be free and open-source, and ContentMine has already provided the PI computing power on their servers. All sensitive files will be encrypted and stored on the PI's local copy with several encrypted backups.