---
title:                "C: स्ट्रिंग को कैपिटलाइज करना"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#क्यों
कई बार हमारे पास स्ट्रिंग का निर्देशानुसार वर्णमाला को कैपिटलाइज करने की आवश्यकता होती है। यह स्ट्रिंग को अलग-अलग प्रकार से प्रदर्शित करने में मदद करता है और यूजर्स को अधिक स्वचालित और आसान पढ़ने में मदद करता है। 

#कैसे करें
आप गोआईप्ले के एक्साम्पल पर चलकर अपना स्ट्रिंग कैपिटलाइज़ कर सकते हैं। नीचे दिए गए कोड ब्लॉक में **fwrite()** फ़ंक्शन का उपयोग करके हम या हमारे प्रोग्राम में दिया गया स्ट्रिंग प्रिंट कर सकते हैं। 

```C
#include <stdio.h> 
#include <string.h> 

int main() 
{ 
   char str[50];
   strcpy(str, "welcome to hindi readers"); 
   printf("Original string: %s\n", str); 

   str[0] = toupper(str[0]); // capitalizing first letter

   for (int i = 1; i < strlen(str); i++)
   {
       if (str[i] == ' ') // if space is encountered
       {
           str[i+1] = toupper(str[i+1]); // capitalize next letter 
       }
   }

   printf("Capitalized string: %s\n", str); 

   return 0;
} 
```

**।। आउटपुट ।।**

मूल स्ट्रिंग: वेलकम टू हिंदी रीडर्स
कैपिटलड स्ट्रिंग: वेलकम टू हिंदी रीडर्स 

#ख़रोच
स्ट्रिंग कैपिटलाइज़ करने के लिए यहां हमने **fwrite()** फ़ंक्शन का उपयोग किया है, लेकिन आप दूसरे फ़ंक्शन भी प्रयोग कर सकते हैं जैसे कि **printf()** और **fgets()**। उदाहरण के लिए, आप **fgets()** को स्ट्रिंग को पढ़ने के लिए प्रयोग कर सकते हैं और उसके बाद स्ट्रिंग को **fwrite()** का उपयोग करके कैपिटलाइज़ कर सकते हैं। यह आपको स्ट्रिंग को अपडेट नहीं करने पर काम करता है लेकिन हमारे द्वारा क