---
title:    "C: डिबग आउटपुट प्रिंट करना"
keywords: ["C"]
---

{{< edit_this_page >}}

## क्यों

क्रियात्मक प्रोग्रामिंग भाषा सी में, डीबग आउटपुट प्रिंट करने का एक बहुत ही महत्वपूर्ण काम है। इसकी मदद से हम अपने कोड को समझते हैं और समस्याओं को सुलझाते हैं। इसलिए, यह एक बहुत ही उपयोगी क्रिया है जो हर प्रोग्रामर को सीखनी चाहिए।

## कैसे करें

डीबग आउटपुट प्रिंट करने के लिए हम `printf` फंक्शन का प्रयोग कर सकते हैं। इस फंक्शन को हम `stdio.h` हैडर फाइल में परिभाषित करते हैं। नीचे दिए गए कोड के उदाहरण में, हम `age` वेरिएबल का मान 25 है और इसे डीबग प्रिंट कर रहे हैं।

```C
#include<stdio.h>
int main()
{
  int age = 25;
  printf("My age is %d years.", age);
  return 0;
}
```

आउटपुट:

```
My age is 25 years.
```

## गहराई में जाएं

डीबग आउटपुट प्रिंट करने के लिए हमें अपने कोड में `printf` फंक्शन का प्रयोग करना होता है। इसके अलावा, हम अपने आउटपुट में वेरिएबल के मान को भी प्रिंट कर सकते हैं। इससे हमें समझने में आसानी होती है कि कोड की क्या स्थिति है और कैसे उसे सुधारा जा सकता है।

## ऐसा देखें

**और भी पढ़ें:**

- [C Programming Tutorial for Beginners](https://www.programiz.com/c-programming)
- [Learn C in Hindi](https://hindi.programiz.com/c-programming)
- [Debugging Techniques in C](https://www.geeksforgeeks.org/debugging-techniques-in-c/)
- [Using printf() for debugging C code](https://stackoverflow.com/questions/20944396/using-printf-for-debugging-c-code)
 
## देखें भी

**सीखने में आसान बनाने के लिए ये भी देखें:**

- [C भाषा में डायनामिक मेमोरी अनुप्रयोग](https://hindi.programiz.com/c-programming/c-dynamic-memory-allocation)
- [कहाँ से शुरू करें: क्र