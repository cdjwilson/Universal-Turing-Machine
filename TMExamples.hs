module TMExamples where

import TM
import UniversalTM

-- following suggestion by Junnan in class
tripletm =
  TM [1 .. 6] "abc" "abc*! " id ' ' '!' trans 1 [6]
  where
    trans = goRight 1 ' ' ' ' 6 ++
            loopRight 1 "*" ++
            goRight 1 'a' '*' 2 ++
            loopRight 2 "a*" ++
            goRight 2 'b' '*' 3 ++
            loopRight 3 "b*" ++
            goRight 3 'c' '*' 4 ++
            loopRight 4 "c*" ++
            goLeft 4 ' ' ' ' 5 ++
            loopLeft 5 "abc*" ++
            goRight 5 '!' '!' 1 

astartm = TM [1,2] "a" "a*!" id ' ' '!' trans 1 [2]
  where trans = goRight 1 ' ' ' ' 2 ++ 
                loopRight 1 "a"

-- >>> accepts astartm "a"
-- True

-- my Universal TM is going to take an encoded turing machine and its input it will have (leftend symbol#blank symbol#start symbol#final state#transitions#@input string and some blank spaces for me to write to)
-- transitions are going to be (state, symbol you want to be looking at, wether you are moving left or right, new state to move to, new symbol to put in the read head you were just looking at)
-- I want to start by getting the information that i am going to write at the end of the tape to keep track (start state/current state, readhead of the input tm)
jake = TM [1 .. 161] "10#,.@" "10#,.*!@olOL`& " id '+' '!' trans 1 [161]
  where trans =                                    -- first part is to write the start state to the place where im holding the state at

                loopRight 1 "10#,." ++             -- move to the right until you see the first blank space
                goRight 1 ' ' ' ' 2 ++             -- when you see the first blank space just move right once and go to a new state
                goRight 2 ' ' '^' 3 ++             -- right a ^ symbol to show that it is where the state is being held on the tape
                loopLeft 3 "10#,.^lo " ++          -- move left to get back to the start
                goRight 3 '!' '!' 4 ++             -- move over once to the leftendsymbol
                loopRight 4 "10" ++                -- move over to the blank symbol
                goRight 4 '#' '#' 5 ++
                loopRight 5 "10" ++                -- move over to start symbol
                goRight 5 '#' '#' 6 ++   
                loopRight 6 "ol" ++
                goRight 6 '#' '#' 37 ++
                goRight 6 '0' 'o' 7 ++             -- if the symbol you are looking at is a zero then move into state 5 and write a o for 0
                loopRight 7 "10#,. " ++            -- move right until you see the ^ symbol to show that you are at the place where the current state is being written
                goRight 7 '^' '^' 8  ++    
                goRight 8 ' ' '0' 3 ++             -- write 0 in the first blank state
                loopRight 8 "10" ++                -- this just gets me to the first blank space
                goRight 6 '1' 'l' 10 ++            -- write an l for 1 and move to new state representing its a 1
                loopRight 10 "10#,. " ++           -- move to where im keeping the state
                goRight 10 '^' '^' 11 ++
                loopRight 11 "10" ++               -- move right to an open space
                goRight 11 ' ' '1' 3 ++            -- write the 1 in the open space an repeat 
                
                -- need to move the read head over from the start symbol to the actual first input
                loopLeft 37 "10#,.lo^OL` " ++      -- move to front of tape
                goRight 37 '!' '!' 38 ++
                loopRight 38 "10#,.lo^OL` " ++     -- move to the readhead which is &
                goRight 38 '&' '&' 39 ++           -- move past start of input
                loopRight 39 "01ol`" ++            -- loop past the first input that is the left end marker
                goRight 39 ',' '@' 12 ++           -- once you move past it and see a comma write a @ to symbolize this is where the readhead is looking to the right of it
                
                
                -- going to be in state 12 and we need something to compare the state we are in and check if its a final state
                loopLeft 12 "10#,.lo^OL`@&+ " ++  -- go back to the start of the tape
                goRight 12 '!' '!' 13 ++           -- move off to leftend symbol
                loopRight 13 "10#,.loOL` " ++      -- move to the ^ symbol where im keeping current state
                goRight 13 '^' '^' 14 ++
                loopRight 14 "ol" ++               -- move to the 0 or 1 spot
                goLeft 14 '0' 'o' 15 ++            -- if the current bit comparing is 0
                goLeft 14 '1' 'l' 22 ++            -- if the current bit comparing is 1

                goRight 14 ' ' ' ' 28 ++           -- if you have compared all of the states then move to the process of checking it is a final state
                
                -- for when it is a 0
                loopLeft 15 "ol01#,^.`OL " ++      -- go to the start of tape
                goRight 15 '!' '!' 16 ++           -- move to lefthand symbol
                loopRight 16 "10ol" ++             -- move past lefthand symbol
                goRight 16 '#' '#' 17 ++           -- move to blank symbol
                loopRight 17 "10ol" ++             -- move past blank symbol
                goRight 17 '#' '#' 18 ++           -- move to start symbol
                loopRight 18 "10ol" ++             -- move past start symbol
                goRight 18 '#' '#' 19 ++           -- move to final symbol
                goRight 19 '#' '#' 13 ++           -- if you have checked all the final states then start over
                loopRight 19 "ol`OL," ++           -- move to the comparing bit of final symbol
                goRight 19 '0' 'o' 20 ++           -- if youre looking at a 0 write a o
                goRight 19 '1' 'L' 20 ++           -- if youre looking for a 0 and see a 1 then place | to symoblize that it has failed but it is a 1
                goRight 19 ',' '`' 19 ++           -- if youre supposed to be looking for a number but are seeing a comma then replace it with ` to show it has failed then move on to the next final state 
                goRight 20 ',' ',' 19 ++           -- if youre looking at a comma restart the process of comparing
                loopRight 20 "01" ++               -- move past the rest of the zeros
                
                -- for when it is a 1
                loopLeft 22 "ol01#,^.OL` " ++      -- go to start of tape
                goRight 22 '!' '!' 23 ++           -- move to lefthand symbol
                loopRight 23 "10ol" ++             -- move past lefthand symbol
                goRight 23 '#' '#' 24 ++           -- move to blank symbol
                loopRight 24 "10ol" ++             -- move past blank symbol
                goRight 24 '#' '#' 25 ++           -- move to start symbol
                loopRight 25 "10ol" ++             -- move past start symbol
                goRight 25 '#' '#' 26 ++           -- move to final symbol
                goRight 26 '#' '#' 13 ++           -- if you have checked all the final states then start over
                loopRight 26 "olOL`," ++           -- move to the comparing bit of final symbol
                goRight 26 '1' 'l' 27 ++           -- if youre looking at 1 write a 1
                goRight 26 '0' 'O'  27 ++          -- if youre looking for a 1 and see a 0 then place O to symbolize that it has failed but it is a 1
                goRight 26 ',' '`' 26 ++           -- if youre supposed to be looking for a number but are seeing a comma then replace it with ` to show it has failed then move on to the next final state
                goRight 27 ',' ',' 26 ++           -- if youre looking at a comma restart the process of comparing
                loopRight 27 "01" ++               -- move past the rest of the zeros
                
                -- for checking that we have a final state
                loopLeft 28 "ol01#,^.OL` " ++      -- go to the start of the tape
                goRight 28 '!' '!' 29 ++           -- move to lefthand symbol
                loopRight 29 "10ol" ++             -- move past lefthand symbol
                goRight 29 '#' '#' 30 ++           -- move to blank sybmol
                loopRight 30 "10ol" ++             -- move past blank symbol
                goRight 30 '#' '#' 31 ++           -- move to start symbol
                loopRight 31 "10ol" ++             -- move past start symbol
                goRight 31 '#' '#' 32 ++           -- move to final symbols
                goRight 32 '#' '#' 34 ++           -- if you see another # then you are done checking all the final states
                loopRight 32 "ol" ++               -- loop over all correct ones
                goRight 32 ',' ',' 161 ++          -- if you see a comma after then move to final state
                goRight 32 'O' 'O' 33 ++           -- move to a different state if it is an O or an L because that means it doesnt match
                goRight 32 'L' 'L' 33 ++ 
                goRight 32 '1' '1' 33 ++
                goRight 32 '0' '0' 33 ++
                goRight 32 '`' '`' 32  ++          -- just compare the next final state if it was a failed one
                loopRight 33 "OLol01" ++           -- it has failed so just loop until the next comma or failed comma
                goRight 33 ',' ',' 32 ++           -- check the next final state
                goRight 33 '`' '`' 32 ++
                
                -- going to reset everything to its original value that hasnt been altered
                loopLeft 34 "ol01#,^.OL` " ++      -- move to the start of the tape
                goRight 34 '!' '!' 35 ++           -- just move right and each spot write the value it translates to
                goRight 35 '1' '1' 35 ++
                goRight 35 '0' '0' 35 ++
                goRight 35 'o' '0' 35 ++
                goRight 35 'l' '1' 35 ++
                goRight 35 '#' '#' 35 ++
                goRight 35 ',' ',' 35 ++
                goRight 35 '^' '^' 35 ++
                goRight 35 '.' '.' 35 ++
                goRight 35 'O' '0' 35 ++
                goRight 35 'L' '1' 35 ++
                goRight 35 '`' ',' 35 ++
                goRight 35 ' ' ' ' 35 ++
                goRight 35 '@' '@' 35 ++
                goRight 35 '&' '&' 35 ++
                goLeft 35 '+' '+' 36 ++

                -- going to start comparing the read head to to the transitions in the tm
                loopLeft 36 "ol01#,^.OL`&@ " ++    -- go to the start of the tape because we dont know where we will be in the tape when we start comparing
                goRight 36 '!' '!' 40 ++           -- move past the start symbol
                loopRight 40 "ol01#,^.OL`& " ++    -- move to the @ symbol which is where the readhead is currently looking
                goRight 40 '@' '@' 41 ++           -- move past @ symbol
                loopRight 41 "01ol" ++             -- move past the current readhead
                goLeft 41 ',' ',' 42 ++            -- once you reach a comma start comparing from right to left of the readhead, start from the 
                loopLeft 42 "0" ++                 -- loop past all the zeros because they dont matter
                goLeft 42 '1' 'l' 45 ++            -- once you see your first 1 move to the state of checking comparing 1's
                goLeft 42 'l' 'l' 43 ++            -- if its a 1 you have already compared then move on to comparing the rest of them
                loopLeft 43 "ol" ++                -- loop until you find a number that hasnt been compared yet
                goLeft 43 '@' '@' 88 ++            -- if you see a @ then you know you are done comparing all the bits in the current readhead
                goLeft 43 '1' 'l' 45 ++            -- if you are comparing a 1 move to the state where you compare 1's
                goLeft 43 '0' 'o' 55 ++            -- if you are comparing a 0 move to the state where you compare 0's
                
                -- for comparing 1's when checking for what the readhead is looking at within transitions in state 45
                loopLeft 45 "ol01#,^.OL`&@ " ++    -- go to the start of the tape
                goRight 45 '!' '!' 46 ++           -- go to the leftend symbol
                loopRight 46 "01" ++               -- go past leftend symbol
                goRight 46 '#' '#' 47 ++           -- go to blank symbol
                loopRight 47 "01" ++               -- go past blank symbol
                goRight 47 '#' '#' 48 ++           -- go to start symbol
                loopRight 48 "01" ++               -- go past start symbol
                goRight 48 '#' '#' 49 ++           -- go to final states
                loopRight 49 "10," ++              -- go past final state
                goRight 49 '#' '#' 50 ++           -- go to transitions
                loopRight 50 "10" ++               -- move past the state part of transition
                goRight 50 '#' '#' 40 ++           -- if you see a hash then we are done checking transitions
                goRight 50 '.' '.' 51 ++           -- move to the symbol you want to be looking at
                loopRight 51 "10olOL" ++           -- move past the symbol then because we want to start comparing from right to left
                goLeft 51 '.' '.' 52 ++            -- start comparing
                loopLeft 52 "ol" ++                -- move past all thost that you have compared that have worked
                goRight 52 '1' 'l' 53 ++           -- if you see a 1 place a l to show that it is correct
                goRight 52 '0' 'O' 54 ++           -- if you see a 0 place a O to show that it has failed
                goRight 52 'O' 'O' 54 ++           -- if you are seeing something that has failed then move on
                goRight 52 'L' 'L' 54 ++
                loopRight 53 "10olOL." ++          -- move past all other states and directions to get to the next transition
                goRight 53 ',' ',' 50 ++           -- since it hasnt failed just move to the next one
                loopRight 54 "10olOL." ++          -- move past all other states for when it failed
                goRight 54 ',' '`' 50 ++           -- since it has failed write a ` to show it has failed
                goRight 54 '`' '`' 50 ++
                
                -- for comparing 0's when checking for what the readhead is looking at within transitions in state 55
                loopLeft 55 "ol01#,^.OL`&@ " ++    -- go to start of tape
                goRight 55 '!' '!' 56 ++           -- go to leftend symbol
                loopRight 56 "01" ++               -- go past leftend symbol
                goRight 56 '#' '#' 57 ++           -- go to blank symbol
                loopRight 57 "01" ++               -- go past blank symbol
                goRight 57 '#' '#' 58 ++           -- go to start symbol
                loopRight 58 "01" ++               -- go past start symbol
                goRight 58 '#' '#' 59 ++           -- go to final states
                loopRight 59 "10," ++              -- go past final states
                goRight 59 '#' '#' 60 ++           -- go to transitions
                loopRight 60 "10" ++               -- move past the state part of the transition
                goRight 60 '#' '#' 40 ++           -- if you are seeing a hash then we are done checking transitions 
                goRight 60 '.' '.' 61 ++           -- if you are seeing a . then move to the symbol part of the transition
                loopRight 61 "10olOL" ++           -- move to the rigth side of the transition
                goLeft 61 '.' '.' 62 ++            -- once you see a dot move left to start comparing
                loopLeft 62 "ol" ++                -- move past all correct comparisions
                goRight 62 '0' 'o' 63 ++           -- if its a 0 then write a o to show that it is correct
                goRight 62 '1' 'L' 64 ++           -- if its a 1 then write a L to show that it has failed
                goRight 62 'O' 'O' 64 ++           -- go to failed state if it has already failed
                goRight 62 'L' 'L' 64 ++
                loopRight 63 "10olOL." ++          -- if it hasnt failed then loop to the next state
                goRight 63 ',' ',' 60 ++           -- keep the comma because it hasnt failed and comapre next state
                loopRight 64 "10olOL." ++          -- if it has failed then move to the next state
                goRight 64 ',' '`' 60 ++           -- since its failed write a ` to show that this state has failed
                goRight 64 '`' '`' 60 ++           -- if its already failed just move on
                
                -- once you are done checking the states then you need to go through and weed out the ones that have failed because they are too long
                loopLeft 88 "ol01OL#,.^`&@ " ++    -- move to left end of tape
                goRight 88 '!' '!' 89 ++           -- go to leftend symbol
                loopRight 89 "01" ++               -- go past leftend symbol
                goRight 89 '#' '#' 90 ++           -- go to blank symbol
                loopRight 90 "01" ++               -- go past blank symbol
                goRight 90 '#' '#' 91 ++           -- go to start symbol
                loopRight 91 "01" ++               -- go past start symbol
                goRight 91 '#' '#' 92 ++           -- go to final states
                loopRight 92 "01," ++              -- go past final states
                goRight 92 '#' '#' 93 ++           -- go to transitions
                goRight 93 '#' '#' 65 ++           -- if you see a hash then you know you are done checking so go on to checking the states
                loopRight 93 "01olOL." ++          -- go to end of transition to check if it has failed
                goRight 93 '`' '`' 93 ++           -- if you see a ` then move on because it has already failed
                goLeft 93 ',' ',' 94 ++            -- if you see a , then check to make sure it doesn't fail
                loopLeft 94 "10olOL." ++           -- move to start of transition
                goRight 94 ',' ',' 95 ++           -- move to state
                goRight 94 '`' '`' 95 ++
                goRight 94 '#' '#' 95 ++
                loopRight 95 "01olOL" ++           -- move past state
                goRight 95 '.' '.' 96 ++           -- move to symbol
                loopRight 96 "ol" ++               -- loop past all compared bits expecting there to be a . afterwards meaning that it hasnt failed
                goRight 96 '.' '.' 97 ++           -- if you see a dot then you know it has hasnt failed
                goRight 96 '1' '1' 98 ++           -- if you see a 1 or 0 then you know it is too long so it has failed
                goRight 96 '0' '0' 98 ++
                loopRight 97 "10." ++              -- go to the next transition
                goRight 97 ',' ',' 93 ++           -- since it hasnt failed just keep the comma
                loopRight 98 "10." ++              -- go to next transition
                goRight 98 ',' '`' 93 ++           -- since it has failed write of the comma with ` to show its a failed transition
                
                -- once you are done comparing the current readhead to the transitions symbols then you will be in state 65 and you will now compare the current state you are in
                loopLeft 65 "ol01OL#,.^`&@ " ++    -- move to left end of the tape
                goRight 65 '!' '!' 66 ++           -- move off start symbol
                loopRight 66 "ol01OL#,.`&@ " ++    -- loop until you get to where the current state is being held
                goRight 66 '^' '^' 67 ++           -- once at read head move over to start comparing
                goRight 67 '1' 'l' 68 ++           -- if its a 1 write a l
                goRight 67 '0' 'o' 78 ++           -- if its a 0 write a o
                loopRight 67 "ol" ++               -- loop to the bit you havent compared yet
                goRight 67 ' ' ' ' 99 ++           -- if you are done comparing all bits then move to the eval of states process
                
                -- for when its a 1 
                loopLeft 68 "ol01OL#,.^`&@ " ++    -- go to start of tape
                goRight 68 '!' '!' 69 ++           -- go to leftend symbol
                loopRight 69 "01" ++               -- go past leftend symbol
                goRight 69 '#' '#' 70 ++           -- go to blank symbol
                loopRight 70 "01" ++               -- go past blank sybmol
                goRight 70 '#' '#' 71 ++           -- go to start symbol
                loopRight 71 "10" ++               -- go past start symbol
                goRight 71 '#' '#' 72 ++           -- go to final states
                loopRight 72 "10," ++              -- go past final states
                goRight 72 '#' '#' 73 ++           -- go to transitions
                loopRight 73 "olOL.01" ++          -- move to the end of the transition to see if it has failed or not
                goRight 73 '#' '#' 66 ++           -- if you have moved past everything and see a hash then we know we are done comparing the transitions so start comparing the next bit
                goLeft 73 ',' ',' 74 ++            -- if its a comma then you know it hasnt failed yet so start comparing the states
                goRight 73 '`' '`' 73 ++           -- if you see a ` then you know it failed so go on to next transition
                loopLeft 74 "01olOL." ++           -- if it hasnt failed the move to the start of the transition
                goRight 74 ',' ',' 75 ++           -- move to the start state
                goRight 74 '`' '`' 75 ++
                goRight 74 '#' '#' 75 ++
                loopRight 75 "ol" ++               -- move past all compared states
                goRight 75 '.' '.' 77 ++           -- if you see a . when you are expecting a number then you know it failed
                goRight 75 '1' 'l' 76 ++           -- write a l because youre looking for a 1
                goRight 75 '0' 'O' 77 ++           -- write a O to show it has failed
                goRight 75 'O' 'O' 77 ++           -- if you see a O or L then you know it has failed
                goRight 75 'L' 'L' 77 ++
                loopRight 76 "10olOL." ++          -- move to the next transition
                goRight 76 ',' ',' 73 ++           -- move over the comma because you havent failed
                loopRight 77 "10olOL." ++          -- move to next transition
                goRight 77 ',' '`' 73 ++           -- write a ` to show that it has failed
                
                -- for when its a 0
                loopLeft 78 "ol01OL#,.^`&@ " ++    -- go to start of tape
                goRight 78 '!' '!' 79 ++           -- go to leftend symbol
                loopRight 79 "01" ++               -- go past leftend symbol
                goRight 79 '#' '#' 80 ++           -- go to blank symbol
                loopRight 80 "01" ++               -- go past blank sybmol
                goRight 80 '#' '#' 81 ++           -- go to start symbol
                loopRight 81 "10" ++               -- go past start symbol
                goRight 81 '#' '#' 82 ++           -- go to final states
                loopRight 82 "10," ++              -- go past final states
                goRight 82 '#' '#' 83 ++           -- go to transitions
                loopRight 83 "olOL.01" ++          -- move to the end of the transition to see if it has failed or not
                goRight 83 '#' '#' 66 ++           -- if you have moved past everything and see a hash then we know we are done comparing the transitions so start comparing the next bit
                goLeft 83 ',' ',' 84 ++            -- if its a comma then you know it hasnt failed yet so start comparing the states
                goRight 83 '`' '`' 83 ++           -- if you see a ` then you know it failed so go on to next transition
                loopLeft 84 "01olOL." ++           -- if it hasnt failed the move to the start of the transition
                goRight 84 ',' ',' 85 ++           -- move to the start state
                goRight 84 '`' '`' 85 ++ 
                goRight 84 '#' '#' 85 ++
                loopRight 85 "ol" ++               -- move past all compared states
                goRight 85 '.' '.' 87 ++           -- if you see a . when you are expecting a number then you know it failed
                goRight 85 '0' 'o' 86 ++           -- write a o because you are looking for a 0
                goRight 85 '1' 'L' 87 ++           -- write a L because it has failed
                goRight 85 'O' 'O' 87 ++           -- if you see a O or L then you know it has failed
                goRight 85 'L' 'L' 87 ++
                loopRight 86 "10olOL." ++          -- move to next transition
                goRight 86 ',' ',' 83 ++           -- move over the comma because you havent failed
                loopRight 87 "10olOL." ++          -- move to next transition
                goRight 87 ',' '`' 83 ++           -- write a ` to show that it has failed
                
                -- now you need to go through and weed out any ones that are too long
                loopLeft 99 "ol01OL#,.^`&@ " ++    -- go to start of tape
                goRight 99 '!' '!' 100 ++          -- go to leftend symbol
                loopRight 100 "01" ++              -- go past leftend symbol
                goRight 100 '#' '#' 101 ++         -- go to blank symbol
                loopRight 101 "01" ++              -- go past blank symbol
                goRight 101 '#' '#' 102 ++         -- go to start symbol
                loopRight 102 "01" ++              -- go past start symbol
                goRight 102 '#' '#' 103 ++         -- go to final states
                loopRight 103 "10," ++             -- go past final states
                goRight 103 '#' '#' 104 ++         -- go to transitions
                goRight 104 '#' '#' 109 ++         -- if you see a hash then you are done going through the transitions
                loopRight 104 "10olOL." ++         -- move to the end of the transition to see if it has failed
                goRight 104 '`' '`' 104 ++         -- if its a ` then it has failed so just move on
                goLeft 104 ',' ',' 105 ++          -- if it is a comma then it hasnt failed yet
                loopLeft 105 "10olOL." ++          -- go to the start of the transition
                goRight 105 ',' ',' 106 ++         -- move over to the state
                goRight 105 '`' '`' 106 ++
                goRight 105 '#' '#' 106 ++
                loopRight 106 "ol" ++              -- loop over the states
                goRight 106 '.' '.' 107 ++         -- if you see a . then it has worked
                goRight 106 '1' '1' 108 ++         -- if you see a 1 or 0 then it has failed
                goRight 106 '0' '0' 108 ++
                loopRight 107 "10olOL." ++         -- go to the end of the transition
                goRight 107 ',' ',' 104 ++         -- if you see a comma then just leave it because it has worked and go on to next transition
                loopRight 108 "10olOL." ++         -- go to the end of the transition
                goRight 108 ',' '`' 104 ++         -- write a ` because it has failed
                
                -- once you have finished checking the state you need to execute a transition, there should only be 1 transition at this point that works
                -- start with writing the new symbol in the spot of the readhead
                loopLeft 109 "ol01OL#,.^`&@ " ++   -- go to start of tape
                goRight 109 '!' '!' 110 ++         -- go to leftend symbol
                loopRight 110 "01" ++              -- go past leftend symbol
                goRight 110 '#' '#' 111 ++         -- go to blank sybmol
                loopRight 111 "01" ++              -- go past blank sybmol
                goRight 111 '#' '#' 112 ++         -- go to start symbol
                loopRight 112 "01" ++              -- go past start symbol
                goRight 112 '#' '#' 113 ++         -- go to final states
                loopRight 113 "01," ++             -- go past final states
                goRight 113 '#' '#' 114 ++         -- go to transitions
                loopRight 114 "ol01OL.`" ++        -- loop until you find the comma the symbolizes the transition we are going to be using
                goRight 114 '#' '#' 160 ++         -- if you loop through and dont find a transition that works then go to a state that doesnt work because it has failed
                goLeft 114 ',' ',' 115 ++          -- if you see a comma then the transition we are looking for is to the left
                goLeft 115 '.' '.' 116 ++          -- move to the symbol
                loopLeft 116 "10ol" ++             -- move to the start of the symbol
                goRight 116 '.' '.' 117 ++
                loopRight 117 "ol" ++              -- go past all processed bits
                goRight 117 '.' '.' 126 ++         -- once you are done writing all the bits
                goRight 117 '1' 'l' 118 ++         -- if its a 1 write a l
                goRight 117 '0' 'o' 122 ++         -- if its a 0 write a o
                
                -- for when its a 1
                loopRight 118 "10olOL.,`&^# " ++   -- loop until you find the readhead
                goRight 118 '@' '@' 119 ++         -- move over to the readhead
                loopRight 119 "10" ++              -- move past all the symbols you have already written
                goLeft 119 ' ' ' ' 120 ++          -- if you loop past everything then go to a state that handles this
                goRight 119 'o' '1' 109 ++         -- if you dont see a space then just write a 1 and start writing the next bit
                goRight 119 'l' '1' 109 ++
                loopLeft 120 "0" ++                -- if you looped past all the spaces then loop back until you see a 1
                goRight 120 '1' '1' 121 ++         -- once you see a 1 move over to the first 0 space
                goRight 121 '0' '1' 109 ++

                -- for when its a 0
                loopRight 122 "10olOL.,`&^# " ++   -- loop until you find the readhead
                goRight 122 '@' '@' 123 ++         -- move over to the readhead
                loopRight 123 "10" ++              -- move past all the symbols you have already written
                goLeft 123 ' ' ' ' 124 ++          -- if you loop past everything then go to a state that handles this
                goRight 123 'o' '0' 109 ++         -- if you dont see a space then just write a 1 and start writing the next bit
                goRight 123 'l' '0' 109 ++
                loopLeft 124 "0" ++                -- if you looped past all the spaces then loop back until you see a 1
                goRight 124 '1' '1' 125 ++         -- once you see a 1 move over to the first 0 space
                goRight 125 '0' '0' 109 ++

                -- once you have finished writing the symbol go over it to fix it if its too small in state 126
                loopRight 126 "10olOL.,`&^# " ++   -- move to the readhead
                goRight 126 '@' '@' 127 ++         -- move past the readhead
                loopRight 127 "10" ++              -- move past all written bits
                goRight 127 ',' ',' 129 ++         -- if you have fixed all the bits then move on to writing the new state
                goRight 127 'o' '0' 128 ++         -- write a 0 to any unwritten bits
                goRight 127 'l' '0' 128 ++
                goRight 128 '1' '0' 128 ++
                goRight 128 '0' '0' 128 ++
                goRight 128 'o' '0' 128 ++
                goRight 128 'l' '0' 128 ++
                goRight 128 ',' ',' 129 ++         -- once you have fixed all the bits move on to writing the new state

                -- after writing and fixing the symbol on the readhead we need to write the new state
                loopLeft 129 "ol01OL#,.^`&@+ " ++   -- go to start of tape
                goRight 129 '!' '!' 130 ++         -- go to leftend symbol
                loopRight 130 "01" ++              -- go past leftend symbol
                goRight 130 '#' '#' 131 ++         -- go to blank sybmol
                loopRight 131 "01" ++              -- go past blank sybmol
                goRight 131 '#' '#' 132 ++         -- go to start symbol
                loopRight 132 "01" ++              -- go past start symbol
                goRight 132 '#' '#' 133 ++         -- go to final states
                loopRight 133 "01," ++             -- go past final states
                goRight 133 '#' '#' 134 ++         -- go to transitions
                loopRight 134 "ol01OL.`" ++        -- loop until you find the comma the symbolizes the transition we are going to be using
                goLeft 134 ',' ',' 135 ++          -- move off the comma to the symbol
                goLeft 135 '.' '.' 136 ++ 
                loopLeft 136 "ol" ++               -- move past the written symbol
                goLeft 136 '.' '.' 137 ++          -- move to the state
                loopLeft 137 "01ol" ++             -- move to the front of the state
                goRight 137 '.' '.' 138 ++
                loopRight 138 "ol" ++              -- move past all written bits
                goRight 138 '.' '.' 143 ++         -- if you have moved past all bits then move on to fixing the bits of the state
                goRight 138 '0' 'o' 139 ++         -- for writing a 0
                goRight 138 '1' 'l' 141 ++         -- for writing a 1

                -- for when its a 0
                loopRight 139 "10ol.,OL`# " ++     -- move over to the ^ which is where we are keeping the state
                goRight 139 '^' '^' 140 ++         -- move past the ^
                loopRight 140 "10" ++              -- move past all the written bits
                goRight 140 'o' '0' 129 ++         -- write a 0 in the first non written bit and move on to the next comparing bit
                goRight 140 'l' '0' 129 ++
                goRight 140 ' ' '0' 129 ++

                -- for when its a 1
                loopRight 141 "10ol.,OL`# " ++     -- move over to the ^ which is where we are keeping the state
                goRight 141 '^' '^' 142 ++         -- move past the ^
                loopRight 142 "10" ++              -- move past all the written bits
                goRight 142 'o' '1' 129 ++         -- write a 0 in the first non written bit and move on to the next comparing bit
                goRight 142 'l' '1' 129 ++
                goRight 142 ' ' '1' 129 ++

                -- after writing the new state then we need to fix it if its too short
                loopRight 143 "ol01OL#,.`&@ " ++   -- move over to the ^ where we are keeping the state
                goRight 143 '^' '^' 144 ++         -- move past the ^
                loopRight 144 "01" ++              -- loop past all written bits
                goRight 144 ' ' ' ' 146 ++         -- if you see a space after then move on to changing the readhead position
                goRight 144 'l' ' ' 145 ++         -- write blanks to any unwritten bits
                goRight 144 'o' ' ' 145 ++
                goRight 145 'l' ' ' 145 ++
                goRight 145 'o' ' ' 145 ++
                goRight 145 ' ' ' ' 146 ++         -- once you have reached a blank symbol then move on to changing the readhead position

                -- for changing the readhead position after writing and fixing the new state
                loopLeft 146 "ol01OL#,.^`&@ " ++   -- go to start of tape
                goRight 146 '!' '!' 147 ++         -- go to leftend symbol
                loopRight 147 "01" ++              -- go past leftend symbol
                goRight 147 '#' '#' 148 ++         -- go to blank sybmol
                loopRight 148 "01" ++              -- go past blank sybmol
                goRight 148 '#' '#' 149 ++         -- go to start symbol
                loopRight 149 "01" ++              -- go past start symbol
                goRight 149 '#' '#' 150 ++         -- go to final states
                loopRight 150 "01," ++             -- go past final states
                goRight 150 '#' '#' 151 ++         -- go to transitions
                loopRight 151 "ol01OL.` " ++
                goLeft 151 ',' ',' 152 ++          -- move over to where moving the read head is kept
                goLeft 152 '.' '.' 153 ++
                loopLeft 153 "ol" ++
                goLeft 153 '.' '.' 154 ++
                loopLeft 154 "ol" ++
                goLeft 154 '.' '.' 155 ++
                goRight 155 '0' '0' 156 ++         -- if its a 0 then it means move the readhead to the right
                goRight 155 '1' '1' 158 ++

                -- for when its a 0 it means move readhead to the right and move on to checking if the current state is in the final state
                loopRight 156 "10olOL.,`&^# " ++   -- loop until the @ which is the readhead
                goRight 156 '@' ',' 157 ++         -- replace the readhead with ,
                loopRight 157 "ol10" ++            -- and move over to the right until the next comma
                goRight 157 ',' '@' 12 ++          -- once at the next comma write the new readhead then move on to checking if we are in a final state

                -- for when its a 1
                loopRight 158 "10olOL.,`&^# " ++  -- loop until the @ which is the readhead
                goLeft 158 '@' ',' 159 ++         -- replace the readhead with a,
                loopLeft 159 "ol01" ++            -- and move over to the left until the next comma
                goRight 159 ',' '@' 12            -- onace at the next comma write the new readhead then move on to checking if we are in a final state

-- >>> encode tripletm
-- "100001#000001#1#011,#1.000001.0.011.000001.,1.010101.0.1.010101.,1.1000011.0.01.010101.,01.1000011.0.01.1000011.,01.010101.0.01.010101.,01.0100011.0.11.010101.,11.0100011.0.11.0100011.,11.010101.0.11.010101.,11.1100011.0.001.010101.,001.1100011.0.001.1100011.,001.010101.0.001.010101.,001.000001.1.101.000001.,101.1000011.1.101.1000011.,101.0100011.1.101.0100011.,101.1100011.1.101.1100011.,101.010101.1.101.010101.,101.100001.0.1.100001.,#"

-- >>> initialConfig tripletm "abc"
-- [1: "!a" "bc"]

-- >>> newConfigs tripletm (initialConfig tripletm "abc")
-- [[2: "!*b" "c"]]

-- >>> inputU astartm "aa"
-- "100001#000001#1#01,#1.000001.0.01.000001.,1.1000011.0.1.1000011.,#                         &1000010000,1000011000,1000011000,0000010000,"

-- >>> getnewConfigntimes 17 tripletm "abc" 
-- [[1: "!b" "c"]]

-- >>> accepts jake (inputU astartm "aab")
-- False

-- >>> accepts jake (inputU tripletm "abcc")
-- False

-- >>> take 1 (getnewConfigntimes 10000 jake (inputU astartm "aa"))
-- [[109: "!100001#000001#1#01,#1.0000Ol.0.01.000001.`l.looooll.0.1.lo00011.,# ^l                      &,1000010000" ",1000011000@10oooll000,0000010000,"]]

-- >>> take 1 (getnewConfigntimes 100000 jake (inputU tripletm "aabbcc"))
-- [[65: "!100001#000001#1#011,#1.0000Ol.0.011.000001.`1.0101Ol.0.1.010101.`1.1Ooooll.0.01.010101.`01.1Ooooll.0.01.1000011.`01.0101Ol.0.01.010101.`01.oloooll.0.11.010101.,11.oloooll.0.11.0100011.,11.0101Ol.0.11.010101.`11.Lloooll.0.001.010101.`001.Lloooll.0.001.1100011.`001.0101Ol.0.001.010101.`001.0000Ol.1.101.000001.`101.1Ooooll.1.101.1000011.`101.oloooll.1.101.0100011.,101.Lloooll.1.101.1100011.`101.0101" "Ol.1.101.010101.`101.1000Ol.0.1.100001.`# ^11                     &,1000010000,0101010000,1000011000,0101010000@oloooll000,1100011000,1100011000,0000010000,"]]

-- >>> writeGraphViz "currentProgress.gv" jake
