#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Aman Mangalore aamangal@ucsc.edu Rajvee Tibrewala ratibrew@ucsc.edu"


execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.
queue := SortedCollection new.

|treeDecode ExitStatus|

FileStream extend [
   nl := Character nl.
   catFile [
      |counter tline|
      counter := 0.
      [self atEnd not] whileTrue: [
         counter := counter + 1.
         tline :=self nextChk.
         counter asString size to: 5 do: [:skip| stdout << ' '].
         
         stdout << counter << ': ' << tline << nl.
      ]
   ]
]

Character extend [
   isPrint [
      ^ (Character space < self) & (self <= $~)
   ]
   digits[
         ^ (self asInteger) > 15
         ]
   visible [
      self isPrint ifTrue: [^' ', self asString, ' ']             
                   ifFalse: [ self digits
                      ifTrue:[((self asInteger) = 256)
                              ifTrue:[ ^ 'EOF']ifFalse:[^ 'x',
                              ((self asInteger printRad: 16)
                               copyFrom: 4 )]
                             ]
                      ifFalse:[
                             
                             ^ 'x0',
                              ((self asInteger printRad: 16)
                               copyFrom: 4)]
                     ]           
   ]
]

Object subclass: Leaf [
   |char counter isleaf|
   
   char [ ^ char ]
   counter [ ^ counter ]
   isleaf [^ isleaf]
   
   Leaf class >> new [
      self dontChange
   ]

   Leaf class >> new: aChar counter: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.      
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      counter := aCount.
      isleaf:=true.
   ]

   <= other [
      ^ (counter < other counter)
      | ((counter = other counter) & (char <= other char))
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << counter
   ]

   inorder: vis prefix: string [
      vis value: char value: string.
   ]
   
   postorder: vis [
      vis  value:char value: isleaf.
   ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar counter: aCount [
      self dontChange
   ]

   Tree class >> new: aChar counter: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar counter: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isleaf:=false.
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: vis prefix: string [
      left inorder: vis prefix: string, '0'.
      right inorder: vis prefix: string, '1'.
   ]
   
   postorder: vis[      
      left postorder: vis .
      right postorder: vis .  
      vis value: char value:isleaf.
   ]
]

Object subclass: ZeroArray [
   |infoArr|

   ZeroArray class >> new [
      self dontChange.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      infoArr := Array new: size.
   ]

   size [
      ^ infoArr size.
   ]

   at: index [
      ^ infoArr at: index + 1.
   ]

   at: index put: value [
      ^ infoArr at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   myValues: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

ftable := ZeroArray new: 257.
etable := ZeroArray new: 257.
ftable at:256 put:1.

Object subclass: BitStream [
   |location byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      location := 1.
   ]
   nextBit [
      location = 1 ifTrue: [
         byte := myStream next.
         location := 9.
      ].
      location := location - 1.
      ^byte value bitAt: location
   ]
   atEnd [
      ^location = 1 and: [myStream atEnd]
   ]
]

Object subclass: OutBits [
   |location currByte myStream|
   OutBits class >> new [
      self dontChange.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   cleared [
      location := 8.
      currByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self cleared.
   ]
   flush [
         myStream nextPutByte: currByte.
         self cleared.

   ]
   writeBit: bit [
      currByte := currByte bitAt: location put: bit.
      location := location - 1.
      location = 0 ifTrue: [
        self flush].
   ]
]


|infile infile2 outfile dcpfile outfilename argC|

dflag := false.
cflag := false.
tflag := false.
uflag := false.

argC:=0.


Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |
   
   (opt = $d) ifTrue: [
   dflag := true.
    ].
    
   (opt = $t) ifTrue: [
   tflag := true.
    ].

  (opt = $c) ifTrue: [
   cflag := true.
   ].

  (opt = $u) ifTrue: [
    uflag := true.
   ].
  
  (arg ~= nil) ifTrue: [
    
     argC = 0 
        ifTrue:[

          dflag ifTrue:[
          stdout <<'input filename: ' << arg <<nl.].
             
          infile := FileStream open: arg
                    mode: FileStream read.
          infile2 := FileStream open: arg
                    mode: FileStream read.
          
         ]
        ifFalse:[
           dflag ifTrue:[
           stdout <<'output filename: ' << arg <<nl.].
            outfile := FileStream open: arg
                       mode: FileStream write.].
  
     argC := argC + 1.
   ].
].


uflag ifFalse:[


[infile atEnd not] whileTrue: [
   |ordChar|
   ordChar := infile next asInteger.
   ftable incr: ordChar.
].

ftable myValues: [:key :counter |
   (counter notNil and: [counter > 0]) ifTrue: [
      queue add: (Leaf new: key counter: counter).
   ]
].

[queue size >1 ] whileTrue: [
   |leftchild rightchild|
   
   leftchild := queue removeFirst.
   rightchild := queue removeFirst.
   newtree := ( Tree new: (leftchild char)
                      counter: (leftchild counter) +  (rightchild counter)
                      left: leftchild right: rightchild).
   queue add: newtree.
].
treeDecode:=queue removeFirst.

treeDecode inorder: [:char :string |
   etable at: char put: string.
] prefix: ''.

binarytree:=''.
treeDecode postorder: [:char :isleaf |

   isleaf  ifFalse:[binarytree:=binarytree,'1'.]
                ifTrue:[
                   bin:=''.
                   char = 256
                        ifTrue:[bin:='000000001']
                        ifFalse:[
                           char=0 ifTrue:[bin:='000000000']
                                  ifFalse:[
                                     8 to:1 by: -1 do:[:i|
                                     bin:=bin,((char bitAt:i)
                                                       printString).
                                   ]
                        ].               
                 ].
                 binarytree:=(binarytree,'0',bin).
              ].
].

tflag ifTrue:[
     stdout <<nl<< '---decoding tree---' << nl.
     stdout << treeDecode << nl<<nl.
     stdout <<'binary decoding tree: '<<nl<<binarytree << nl.

     stdout <<nl<< '---encoding table---' << nl.
     etable myValues: [:key :counter |
     (counter notNil and: [counter > 0]) ifTrue: [
        stdout <<  key asCharacter visible << '     '
               << (ftable at: key) <<'    '<<counter
               << nl.
     ].
    ].
].


cflag  ifTrue:[ 

outfile~=nil ifTrue:[
   
    dflag ifTrue: [
    stdout <<nl<< 'cflag = '<<cflag 
          <<'  output file = true'<< nl.].

   send := OutBits new: outfile.    
   1 to: (binarytree size) by: 1 do: [:index |
         bit:= ((binarytree at: index) asInteger) - 48.
         send writeBit: bit.
         ].
 
   send writeBit: 1.

   [infile2 atEnd not] whileTrue: [
      |ordChar2|
       ordChar2 := infile2 next asInteger.
       code:= (etable at: ordChar2). 
       1 to: (code size) by: 1 do: [:index |
         bit:= ((code at: index) asInteger) - 48.
         send writeBit: bit.
          ].
       ].

   eof:= (etable at: 256).
   1 to: (eof size) by: 1 do: [:index |
         bit:= ((eof at: index) asInteger) - 48.
         send writeBit: bit.].     
   send flush.
]
ifFalse:[
   dflag ifTrue:[
   stdout <<nl<< 'cflag = '<<cflag
              <<' without output file'<< nl.].
    
   stdout <<nl<< '---encoded input in binary---' << nl.
   stdout <<binarytree<<1.      
    
     [infile2 atEnd not] whileTrue: [
           |ordChar2|
           ordChar2 := infile2 next asInteger.
           code:= (etable at: ordChar2).
           stdout <<code. 
        ].
      eof:= (etable at: 256).
      stdout<<eof.
   ].    
 ].
].


(uflag) ifTrue:[
   |iseof send stack|
   iseof:=false.

   dflag ifTrue: [
   stdout<<'doing decompression'<<nl.].

   send := OutBits new: outfile.  
   stack := OrderedCollection new.
   fileBits := BitStream new: infile.
   treeup:=false.
   reconTree:=''.
   node:=''.   
   
   [(fileBits atEnd not) & (iseof not)] whileTrue: [

     tnode:=fileBits nextBit.
     leafc:=0.

     treeup ifFalse:[

        tnode=0 ifTrue:[

           8 to:1 by:-1 do:[:i|
              bit:=fileBits nextBit.
              
            leafc:=leafc bitAt:i put:bit.                        
           ].

           leafc=0 ifTrue:[
             specialbit:=fileBits nextBit.
             specialbit=1 ifTrue:[
              leafc:=leafc bitAt:9 put:1.
             ].
           ].
         
          stack add: (Leaf new: leafc counter: '').

        ].

        tnode=1 ifTrue:[
                 
           stack size>1 ifTrue:[
           rightchild:=stack removeLast.
           leftchild:=stack removeLast.
           
           newtree := (Tree new: -1
                      counter: ''
                      left: leftchild right: rightchild).
             stack add: newtree.
           ] ifFalse:[
             treeup:=true.
             
             reconTree:=stack removeLast.
             node:=reconTree.
     
 
           ].
        ].
     ]
     ifTrue:[
        
       node isleaf ifTrue:[
                ((node char)=256) ifTrue:[
                    iseof:=true.
                    ] ifFalse:[
                     stdout nextPut: (Character value: (node char)).
                  node:=reconTree.].
               ]ifFalse:[
        tnode=0 ifTrue:[
                
                node:=node left.

                node isleaf ifTrue:[
                iseof not ifTrue:[  
                    ((node char)=256) ifTrue:[
                    iseof:=true.


                    ] ifFalse:[

                     outfile=nil ifTrue:[

                     stdout nextPut: (Character value: (node char)).
                     ]
                     ifFalse:[
                        8 to: 1 by: -1 do: [:index |
                           bit:= (node char) bitAt: index.
                           send writeBit: bit.
                        ].
                     ].
                     node:=reconTree.

                    ].
                ].
               ]
           ] ifFalse:[
               
               node:=node right.

               node isleaf ifTrue:[
                iseof not ifTrue:[
                    ((node char)=256) ifTrue:[
                    iseof:=true.

                    ] ifFalse:[

                     outfile=nil ifTrue:[

                     stdout nextPut: (Character value: (node char)).
                     ]
                     ifFalse:[
                        8 to: 1 by: -1 do: [:index |
                           bit:= (node char) bitAt: index.
                           send writeBit: bit.
                        ].
                     ].
                    node:=reconTree.

                    ].
                ].
               ]
           ].
       ].
     ].
   ].
].  


infile close.
outfile~=nil ifTrue:[
outfile close.
].
ExitStatus:=0.
dflag ifTrue: [
stdout<<nl<<'Exit Status: '<<ExitStatus<<nl.
stdout<<'Finished '<<nl.
].
