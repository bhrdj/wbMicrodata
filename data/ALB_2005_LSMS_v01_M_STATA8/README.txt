Community files

community_all: the community-level variable is m0_q00 
(PSU: Primary Sampling Unit). There is only one EA (Enumeration Area) 
per PSU/community. No communities are duplicates. 

To merge with household-level data it is enough to merge by m0_q00.

community_dups: there are 22 PSU/community which comprise more 
than one EA. In addition, there are two villages in one community 
in the same EA. They have different m0_q01 codes (questionnaire number) 
and names (variable m003b). 

To merge with household-level data, merge by m0_q00 id_ea m0_q01.
