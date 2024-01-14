---
title:                "C: डीबग प्रिंटिंग"
simple_title:         "डीबग प्रिंटिंग"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों
डीबग आउटपुट को प्रिंट करने का कार्य क्यों करना है, यह केवल 1-2 वाक्यों में समझाया जा सकता है।

## कैसे करें
आउटपुट को प्रिंट करने के लिए आपको कोड में `printf()` या `fprintf()` फंक्शन का उपयोग करना होगा। यहां एक उदाहरण है:

```C
#include <stdio.h>

int main()
{
    int num = 10;
    char str[] = "Hello World!";
    
    // Printing integer variable
    printf("Value of num is %d\n", num);
    // Printing string variable
    printf("String: %s\n", str);
    
    return 0;
}
```

आउटपुट:
```
Value of num is 10
String: Hello World!
```
इस उदाहरण में, हमने `printf()` फंक्शन का उपयोग करके एक नंबर और एक स्ट्रिंग को प्रिंट किया है। आप इस तरह से किसी भी प्रकार की वेरिएबल को प्रिंट कर सकते हैं।

## गहराई में पता करें
जब आप अपने कोड में डीबग आउटपुट स्टेटमेंट डालते हैं, तो आप अपने प्रोग्राम के भागों को सुलझा सकते हैं और अपने कोड को सही कर सकते हैं। यह आपको एक अधिक सक्रिय डेवलपर बनाता है। आप डीबग आउटपुट को इस्तेमाल करके अपने कोड के कई दुबारा चलने की जरूरत नहीं होती।

## देखें भी
* [डीबग आउटपुट का उपयोग करने के लिए पूरा गाइड](https://www.tutorialspoint.com/cprogramming/c_input_output.htm)
* [डीबगिंग सिर्फ दूर तक नहीं है](https://medium.com/@saigowthamr/mail-me-if-you-are-lost-during-debugging-cf1bba78bf63)
* [कोड को कैसे डीबग करें](https://www.javatpoint.com/cpp-debugging-program)