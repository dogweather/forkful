---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

**## क्या और क्यों?**

रैन्डम नंबर जेनरेशन का मतलब होता है किसी ऐसे संख्या को जन्माना जो पूरी तरह अनिर्दिष्ट (अनुमानातीत) हो। प्रोग्रामर्स इसे उपयोग करते हैं क्योंकि ऐसे अवसर आते हैं जब उन्हें एक रैन्डम आउटकम की आवश्यकता होती है, उदाहरण के लिए गेम्स, एनक्रिप्शन, और टेस्टिंग। 

**## कैसे होता है?:**

C प्रोग्रामिंग में, `<stdlib.h>` लाइब्ररी का `rand()` फ़ंक्शन इस्तेमाल करके रैन्डम नंबर उत्पन्न किया जा सकता है। एक सामान्य उदाहरण नीचे दिया गया है:

```C
#include <stdio.h> 
#include <stdlib.h> 
#include <time.h> 

int main() 
{ 
    // सीड सेट करने के लिए करंट टाइम का इस्तेमाल
    srand(time(0)); 
  
    for(int i = 0; i<5; i++) 
        printf(" %d ", rand()); 
  
   return 0; 
} 
```

इसे चलाने से हमें हर बार एक नया रैन्डम संख्या का समूह मिलेगा।

**## गहराई में जानकारी:**

C प्रोग्रामिंग का `rand()` फ़ंक्शन 1970 के दशक में पहली बार पेश किया गया था। अगर आपको और अधिक तर्कसंगत रैन्डम नंबर की आवश्यकता होती है, तो आप GNU C Library (glibc) के `random()` फ़ंक्शन का भी उपयोग कर सकते हैं, जो ऊचा वालीटी वाले रैन्डम नंबर जनरेट करता है। ध्यान दें कि `rand()` और `srand()` फ़ंक्शन्स पूर्णांक प्रकार के नंबर लौटाते हैं, अगर आपको दशमलव नंबरों की आवश्यकता हो तो आपको उन्हें स्वयं कनवर्ट करना होगा।

**## देखना भी:**

रैन्डम नंबर जेनरेशन के बारे में और अधिक जानकारी के लिए, नीचे 
दिये गए स्रोत की और एक नज़र डालें:
- [C Library - <stdlib.h>](https://www.tutorialspoint.com/c_standard_library/stdlib_h.htm)
- [C Programming/Standard libraries](https://en.wikibooks.org/wiki/C_Programming/Standard_libraries)
- [Number Generator in C](https://fresh2refresh.com/c-programming/c-projects/number-generator/)