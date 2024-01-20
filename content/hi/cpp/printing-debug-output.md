---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

"Debug Output" मूलतः एक प्रोग्राम के आंतरिक प्रक्रियाओं का संकेत है, जिसे डीबग करने, कि प्रोग्राम कहां और कैसे चल उठा है, के लिए प्रिंट किया जाता है। "Debug Outputs" बड़े प्रोग्राम्स को समझने, डीबग और विश्वसनीयता को बढ़ाने में महत्त्वपूर्ण है।

## कैसे करें:

डीबग आउटपुट को प्रिंट करने का आम तरीका "cout" और "cerr" फ़ंक्शन का उपयोग करना है। इसकी एक साधारण उदाहरण नीचे दी गई है:

```C++
#include<iostream>
using namespace std;

int main() {
    cout<<"Hello, World!\n";
    cerr<<"Debug: Start of program\n";
    int testVar = 5;
    cerr<<"Debug: Value of variable at line "<<__LINE__<<" is "<<testVar<<"\n";
    return 0;
}
```

ऊपर कोड का आउटपुट कुछ ऐसा दिखाई देगा:

```
Hello, World!
Debug: Start of program
Debug: Value of variable at line 7 is 5
```

## गहराई में:

डीबग आउटपुट का इतिहास सी++ के पुराने गैर-मानक "printf()" फंक्शन के साथ शुरू होता है जो कि अभी भी उपयोग होता है। आधुनिक परिवेशन में, "cerr" और "cout" का उपयोग करने के लिए प्रोत्साहन दिया जाता है, क्योंकि इन्हें अधिक खुलापन और बदलाव करने की क्षमता प्रदान करता है।

cerr और cout के अलावा, आप लॉग फ़ाइल में डीबग। जैसे कि "fstream" क्लास का उपयोग करके आउटपुट रेकॉर्ड कर सकते हैं।

## विशेष दृष्टिकोण:

डीबगगिंग के भीतर और बाहर विशेष ध्यान देने योग्य कुछ संसाधन नीचे दिए गए हैं:

3. [Understanding Streambuf in C++](http://www.cplusplus.com/reference/streambuf/)
4. [Exceptions and Error handling in C++](https://www.tutorialspoint.com/cplusplus/cpp_exceptions_handling.htm)