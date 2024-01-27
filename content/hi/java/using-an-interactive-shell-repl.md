---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
date:                  2024-01-26T04:16:21.845068-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
REPL (Read-Eval-Print Loop) एक इंटरएक्टिव शेल है जो एकल उपयोगकर्ता इनपुट को प्रोसेस करता है, कोड को निष्पादित करता है, और परिणाम लौटाता है। प्रोग्रामर इसका उपयोग त्वरित प्रयोगों, डिबगिंग, या सीखने के लिए करते हैं, क्योंकि यह तत्काल प्रतिक्रिया और पुनरावृत्ति की अनुमति देता है।

## कैसे करें:
Java 9 में परिचय किए गए `jshell` उपकरण के साथ जावा में एक REPL शुरू करना सरल है। यहां बताया गया है कि इसे कैसे प्राप्त करें और एक आधारभूत सत्र कैसे शुरू करें:

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  बना दिया गया विधि sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

किसी भी समय `/exit` के साथ बाहर निकलें।

```Java
jshell> /exit
|  अलविदा
```

## गहन अध्ययन
`jshell` से पहले, जावा प्रोग्रामर्स के पास एक आधिकारिक REPL नहीं था, पाइथन या रूबी डेव्स के विपरीत। वे आईडीई या पूर्ण कार्यक्रमों का उपयोग करते थे, यहां तक कि तुच्छ कार्यों के लिए भी। जावा 9 के रूप में `jshell` एक खेल-बदलाव वाला था, जिसने उस अंतर को पाट दिया।

विकल्प में ऑनलाइन कम्पाइलर या IDE प्लगइन्स शामिल हैं, लेकिन वे `jshell` की तत्कालता के बराबर नहीं हैं। आंतरिक रूप से, `jshell` जावा कंपाइलर API का उपयोग करके कोड खंडों को निष्पादित करता है, जो काफी अच्छा है। यह केवल एक प्लेग्राउंड से अधिक है—यह पुस्तकालयों को आयात कर सकता है, कक्षाएं परिभाषित कर सकता है, और अधिक। यह इसे प्रोटोटाइपिंग के लिए एक मजबूत उपकरण बनाता है।

## देखें भी
- [JShell उपयोगकर्ता मार्गदर्शिका](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java प्लेटफ़ॉर्म, स्टैंडर्ड एडिशन टूल्स रेफरेंस](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java कंपाइलर API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)