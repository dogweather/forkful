---
title:                "C: स्ट्रिंग्स को जोड़ना"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप किसी बड़े स्ट्रिंग को प्रिंट करना चाहते हैं तो आपको एक abstract data type से मूल्य प्राप्त करके उसे प्रिंट करना होगा. और यदि आपको इस स्ट्रिंग के साथ दूसरे स्ट्रिंग को जोड़ना है, तो आपको स्ट्रिंग को जोड़ने के लिए एक अनुभवी समूह का उपयोग करना होगा.

## कैसे करें

आपको स्ट्रिंग को जोड़ने के लिए स्ट्रिंग के अंत में दो आंकड़ों के साथ एक नया स्ट्रिंग बनाना होगा. नीचे दिए गए कोड ब्लॉक में एक उदाहरण है:

```C
//कोड उदाहरण
char str1[20] = "Hello";
char str2[] = "World";
char result[30];

//strcat () फ़ंक्शन से स्ट्रिंगों को जोड़ें
strcat(result,str1);
strcat(result," ");
strcat(result,str2);
printf("%s", result);

// आउटपुट: Hello World
```

## गहराई में खोज

C में स्ट्रिंगों को जोड़ने के लिए, स्ट्रिंग को एक्सेस करने और उस स्ट्रिंग को तय करने के लिए कुछ विशेष तरीके हो सकते हैं. इनमे से कुछ तरीकों के बारे में अधिक जानकारी के लिए, नीचे दिए गए लिंक्स का उपयोग करें:

- [String Concatenation in C](https://www.programiz.com/c-programming/library-function/string.h/strcat)
- [String Manipulation Functions in C](https://www.geeksforgeeks.org/string-manipulation-in-c-without-using-string-h/)
- [Arrays in C](https://www.tutorialspoint.com/cprogramming/c_arrays.htm)

## देखें भी

- [Pointers in C](https://www.hackerearth.com/practice/notes/pointers-in-c/)
- [Memory Allocation in C](https://www.geeksforgeeks.org/dynamic-memory-allocation-in-c-using-malloc-calloc-free-and-realloc/) 
- [C Language Tutorials in Hindi](https://www.studytonight.com/c/)