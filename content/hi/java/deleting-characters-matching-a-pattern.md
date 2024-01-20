---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# जावा में एक पैटर्न से मिलते अक्षरों को हटाना

## क्या और क्यों?
एक पैटर्न से मिलते अक्षरों को हटाना इसका मतलब है कि आप किसी विशिष्ट अक्षर या अक्षरों का समूह को किसी शब्द से निकालते हैं। यह उदाहरण स्वरूप, अनचाहे ह्वाइट स्पेस को निकालने, या अप्रयोजित अक्षरों को हटाने के लिए किया जाता है। 

## कैसे करें:
जावा 8 अथवा आधुनिक संस्करणों में, `String` class का `replaceAll()` function इसे करने के लिए उपयोग किया जा सकता है। 

```Java

public class Main 
{
    public static void main(String[] args) 
    {
        String str = "Hello, World!";
        String newStr = str.replaceAll("[^a-zA-Z ]", "");
        System.out.println(newStr);
    }
}

```

उपरोक्त कोड का आउटपुट होगा:

```
Hello World
```

## गहरी बातचीत:
`replaceAll()` का function उन सभी अक्षरों को हटाने के लिए उपयोग किया जाता है जो एक विशिष्ट पैटर्न से मेल नहीं खाते। यह जावा 1.4 से उपलब्ध है।

वैकल्पिक तरीके में आप `StringBuilder` या `StringBuffer` class का upyog kar सकते हैं। लेकिन, `replaceAll()` सबसे आसान, कम कोड लाइन्स में और स्रोत को समझने में आसान way है। 

## देखें भी:
1. Oracle जावा Doc `replaceAll()` : [link](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
2. Stack Overflow चर्चा: [link](https://stackoverflow.com/questions/8923398/how-to-remove-certain-characters-from-a-string-in-java)