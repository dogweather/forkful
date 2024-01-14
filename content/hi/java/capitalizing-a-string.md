---
title:                "Java: स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी व्यक्ति जब भी Java में string को capitalize करता है, तो वह उसके लिए इसके उपयोग से जुड़े हुए किसी काम को आसान बनाना चाहता है।

## कैसे करें

आप string को capitalize करने के लिए Java में `toUpperCase()` और `charAt()` जैसे मेथड्स का उपयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक में एक उदाहरण दिखाया गया है।

```Java
String str = "hello";
char firstChar = str.charAt(0);
String capitalizedStr = Character.toUpperCase(firstChar) + str.substring(1);
System.out.println(capitalizedStr);
```

आपको नया String `Hello` मिलेगा जो कि `hello` को capitalize किया हुआ है।

## गहरे समझ करें

समझने के लिए, आप capitalize करने के तरीके के बारे में और गहराई से जान सकते हैं। जैसे कि, आप `StringBuilder` और `StringBuffer` जैसे दो अन्य तरीकों से भी capitalize कर सकते हैं। आप मार्कडाउन में भी capitalize string के लिए अन्य मेथड्स और इसके आवेदन को देख सकते हैं।

## देखें भी

- [Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java StringBuilder documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuilder.html)
- [Java StringBuffer documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html)