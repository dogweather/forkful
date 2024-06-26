---
date: 2024-01-26 04:16:21.845068-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Java 9 \u092E\u0947\
  \u0902 \u092A\u0930\u093F\u091A\u092F \u0915\u093F\u090F \u0917\u090F `jshell` \u0909\
  \u092A\u0915\u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 \u091C\u093E\u0935\u093E\
  \ \u092E\u0947\u0902 \u090F\u0915 REPL \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\
  \u093E \u0938\u0930\u0932 \u0939\u0948\u0964 \u092F\u0939\u093E\u0902 \u092C\u0924\
  \u093E\u092F\u093E \u0917\u092F\u093E \u0939\u0948 \u0915\u093F \u0907\u0938\u0947\
  \ \u0915\u0948\u0938\u0947 \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\
  \u0947\u0902 \u0914\u0930 \u090F\u0915 \u0906\u0927\u093E\u0930\u092D\u0942\u0924\
  \ \u0938\u0924\u094D\u0930\u2026"
lastmod: '2024-03-13T22:44:52.116345-06:00'
model: gpt-4-0125-preview
summary: "Java 9 \u092E\u0947\u0902 \u092A\u0930\u093F\u091A\u092F \u0915\u093F\u090F\
  \ \u0917\u090F `jshell` \u0909\u092A\u0915\u0930\u0923 \u0915\u0947 \u0938\u093E\
  \u0925 \u091C\u093E\u0935\u093E \u092E\u0947\u0902 \u090F\u0915 REPL \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u0939\u0948\u0964 \u092F\
  \u0939\u093E\u0902 \u092C\u0924\u093E\u092F\u093E \u0917\u092F\u093E \u0939\u0948\
  \ \u0915\u093F \u0907\u0938\u0947 \u0915\u0948\u0938\u0947 \u092A\u094D\u0930\u093E\
  \u092A\u094D\u0924 \u0915\u0930\u0947\u0902 \u0914\u0930 \u090F\u0915 \u0906\u0927\
  \u093E\u0930\u092D\u0942\u0924 \u0938\u0924\u094D\u0930 \u0915\u0948\u0938\u0947\
  \ \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902."
title: "\u0907\u0902\u091F\u0930\u0948\u0915\u094D\u091F\u093F\u0935 \u0936\u0947\u0932\
  \ (REPL) \u0915\u093E \u0909\u092A\u092F\u094B\u0917"
weight: 34
---

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
