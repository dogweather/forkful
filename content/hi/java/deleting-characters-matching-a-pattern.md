---
title:                "Java: एक पैटर्न से मेल खाते हुए अक्षरों को हटाने"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

ऐसा होने के लिए कि लोग किसी पैटर्न से मेल खाने वाले अक्षरों को हटाने में लगे |

## कैसे करें

```Java
// कोड उदाहरण हटाने के लिए पैटर्न से मेल खाने वाले अक्षरों को

String word = "हेल्लो";

// String.replaceAll का उपयोग करें
// यह method स्ट्रिंग में दिए गए पैटर्न को उपयोग करके सभी मिलती हुई स्ट्रिंग्स को रिप्लेस करता है |

String result = word.replaceAll("ल्ल", "");

System.out.println(result); // हेो
```

## गहराई में समझना

अक्सर हमें स्ट्रिंग्स में पैटर्न के मेल खाने वाले विशेष अक्षरों को हटाने की जरूरत होती है। इस लेख में, हमने String.replaceAll क्या है और यह कैसे हमें आसानी से पैटर्न से मेल खाने वाले अक्षरों को हटाने में मदद करता है |

## देखें भी

- [Java  स्ट्रिंग विधि निकालें](https://www.w3schools.com/java/java_ref_string.asp)
- [Regular Expressions का उपयोग करना सीखें](https://www.javatpoint.com/java-regex)
- [Java String.replaceAll() की विस्तृत जानकारी](https://www.geeksforgeeks.org/java-string-replaceall-method-example/)