---
title:    "Java: श्रृंखला की लंबाई को खोजना"
keywords: ["Java"]
---

{{< edit_this_page >}}

## क्यों

जब हम किसी पाठ को कंप्यूटर में प्रोग्रामिंग करने के लिए इस्तेमाल करते हैं तो हमें पता होना चाहिए कि यह पाठ कितनी लंबी है। तो आज हम आपको बताएंगे कि आप किस तरह एक स्ट्रिंग की लंबाई को जान सकते हैं।

## कैसे करे

अगर आपको एक स्ट्रिंग की लंबाई को जानना है तो आप ```str.length() ``` को प्रोग्राम में इस्तेमाल कर सकते हैं। यह आपको स्ट्रिंग की लंबाई को जानने में मदद करेगा। नीचे दिए गए कोड ब्लॉक में दिखाया गया है कि कैसे आप इस्तेमाल कर सकते हैं:

```Java
String str = "मेरा स्ट्रिंग";
int length = str.length();
System.out.println(length); // Output: 10
```

## गहराई में जाएं

जब हम एक स्ट्रिंग की लंबाई को जानने के लिए ```length() ``` मेथड का इस्तेमाल करते हैं तो हम असल में क्या कर रहे हैं? स्ट्रिंग में हमेशा हर एक चरित्र के बाद एक निर्दिष्ट मान होता है जिसे बताता है कि उस स्ट्रिंग का कौन सा चरित्र है। जब हम स्ट्रिंग की लंबाई को जानने के लिए ```length() ``` मेथड का इस्तेमाल करते हैं, तो हम अंत तक पहुंचकर हर चरित्र की संख्या को पाते हैं।

## देखें भी

[Coding With Naina - Java Basics](https://www.youtube.com/watch?v=5hBfQKQiqPM)

[Tutorialspoint Java String length() method](https://www.tutorialspoint.com/java/lang/string_length.htm)

[GeeksforGeeks - Length of a string in Java](https://www.geeksforgeeks.org/length-of-a-string-in-java/)