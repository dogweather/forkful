---
title:                "Bash: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

विकासकों और प्रोग्रामर्स के लिए Temporary files का उपयोग उनकी प्रोग्राम्स को बदलने या अपडेट करने के दौरान करना जरूरी हो सकता है। Temporary files उनके काम को आसान और स्मूथ बनाते हैं, और सुनिश्चित करते हैं कि कोई भी डेटा चूर्ण न हों। 

## कैसे करें 

आप temporary file कैसे बना सकते हैं, इसके लिए हमें ```mktemp``` command का उपयोग करना होता है। नीचे दिखाए गए कोड में हम एक temporary फ़ाइल बनाते हैं जो एक unique नाम से शुरू होती है। 

```Bash 
tempfile=$(mktemp) 
echo "This is a temporary file" >> $tempfile 
cat $tempfile 
rm $tempfile 
```

उपरोक्त कोड का output नीचे दिया गया है। 

```Bash 
This is a temporary file 
```

## गहराई में जाएँ 

जब हम temporary file बनाते हैं, तो default रूप से यह ```/tmp``` directory में बनती है। हम ऐसा करते हैं क्यूंकि यह एक ताजगी तथा temporary file को जल्दी से delete कर सकने वाली जगह होती है। आप भी अपने temporary file को किसी और directory में बना सकते हैं, यह ```mktemp``` command में ```-p``` ऑप्शन का उपयोग करके किया जा सकता है। 

## देखें भी 

[बाश प्रोग्रामिंग के बेहतरीन tutorials](https://www.geeksforgeeks.org/bash-scripting-tutorial/) 
[पावरशेल स्क्रिप्टिंग में temporary files का उपयोग कैसे करें](https://www.lifewire.com/creating-temp-files-in-powershell-scripts-3897488) 
[Temporary files क्या हैं और उनका उपयोग क्यों किया जाता है](https://www.computerhope.com/jargon/t/tempfile.htm)