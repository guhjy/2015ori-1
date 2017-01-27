In order to go from `savedrecs.txt` to `full_frame.csv` the following terminal script was used

```bash
echo emails > full_frame.csv
cat savedrecs.txt | grep -P 'EM\s' | sed 's/EM\s//g' | sed 's/;//g' | tr '[:upper:]' '[:lower:]' | sed 's/\s/\n/g' | sort | uniq >> full_frame.csv
```

## Notes for the renewed sampling frame

### General procedure
- In general, there were some people without a profile link so that I couldn't collect their email-addresses and some with profiles but without email address
- In general, when there was a person without a doctor title in their name but not listed as a PhD, I added unclear
- At the end, I removed doubled email in this new list (e.g., people affiliated with multiple departments) and cross-checked with the sampling frame from the first round

### University-specific notes

#### UvA:
- I included all people from the list
	* Positions unclear
	* I marked all people without a drs/dr and prof as unclear

#### VU:
- I included all people from the following groups on the website
	* Biological Psychology: Staff, Postdocs, PhD students
	* Clinical, Neuro- & Developmental Psychology - Section Clinical Developmental Psychology: "No page found"
	* Clinical, Neuro- & Developmental Psychology - Section Clinical Neuropsychology: Head of the section, Academic staff, Postdocs, Researchers, PhD students, PhD students external
	* Clinical, Neuro- & Developmental Psychology - Section Clinical Psychology: Head of the department, Academic staff (this also included people classified as teaching staff but the one I checked also had publications so I just included teaching staff, too), Researchers, PhD students
	* Experimental and Applied Psychology - Section Cognitive Psychology: Head of the section, Staff, Postdocs, Researchers, PhD Students
	* Experimental and Applied Psychology - Social Psychology: Staff, internal affiliates, PhD Students, External PhD Students

- I did not include all people from the following groups on the website
	* Biological Psychology: Research associates/assistants, Secretarial staff, Other
	* Clinical, Neuro- & Developmental Psychology - Section Clinical Developmental Psychology: "No page found"
	* Clinical, Neuro- & Developmental Psychology - Section Clinical Neuropsychology: Secretarial staff
	* Clinical, Neuro- & Developmental Psychology - Section Clinical Psychology: Secretarial staff
	* Experimental and Applied Psychology - Section Cognitive Psychology: Secretarial staff
	* Experimental and Applied Psychology - Social Psychology: Research assistants, Secretarial staff, External affiliates

#### University of Groningen:
- I included all people from the following groups on the website except for people from the group "Secretariat Management Psychology: All secretaries"
- Within each group, I included all people except for the secretaries
- For some people, the function was not listed (could be research assistants?)

#### University of Twente:
- Within each group, I included all people except for the secretaries, support staff, and interns/student assistants
- For some people, the function was unclear (for instance, is a lecturer without a "dr" in their name a phd student?)

#### Utrecht University:
- I did not include people with the following positions
	* Administrative Assistant
	* Administrator
	* Department Manager
	* ICT Support Assistant
	* Intern
	* Managing Director
	* Porter / Receptionist / Telephone Operator
	* Research and Education Assistant
	* Research Assistant
	* Secretary
	* Secretary to the Board
	* Student Affairs Assistant
	* Student Assistant
	* Technical Support Assistant for Teaching and Research

#### Tilburg University:
- I did not include people with the following positions
	* Studentassistent
	* Secretary
	* Managementsup Services - Schools

#### Leiden University:
- I did not include people with the following positions
	* Secretary
	* Staff Member Communication
	* Education and Research Staff Member
	* Skills Instructor
	* Project Coordinator

#### Maastricht University:
- Many profiles without email address
- For Clinical Psychological Science and Neuropsychology and Psychopharmacology: No further information without deeper investigation (which I did not do)
- For Work and Social Psychology: I did not include people with the following positions
	* Administrative Staff

#### Radboud University Nijmegen:
- There are three institutes in the faculty of Social Sciences, but none of them has psychology in there -> So, I did not include them
- However, there is medical psychology and psychiatry in "Medical Sciences". So, I included them

#### Erasmus University Rotterdam:
- I did not know what to do with "Irregular Staff" so I also marked them as "unclear"

#### Open University
- No email addresses available for the staff
