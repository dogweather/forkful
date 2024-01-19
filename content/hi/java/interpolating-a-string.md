---
title:                "एक स्ट्रिंग को इंटरपोलेट करना"
html_title:           "Java: एक स्ट्रिंग को इंटरपोलेट करना"
simple_title:         "एक स्ट्रिंग को इंटरपोलेट करना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामर अक्सर स्ट्रिंग को इंटरपोलेट करते हैं, जिससे वे डायनामिक डेटा को स्ट्रिंग में लगातार जोड़ सकते हैं। इससे उन्हें दिखाया जाता है कि प्रोग्राम कैसे काम कर रहा है और किसी विशेष डेटा को स्ट्रिंग में प्रदर्शित कैसे किया जा सकता है।

## कैसे?

```java
String name = "अमित";
int age = 25;
System.out.println("मेरा नाम है " + name + " और मैं " + age + " साल का हूँ।");
```

आउटपुट:
```मेरा नाम है अमित और मैं 25 साल का हूँ।```

## गहराई में

(1) स्ट्रिंग इंटरपोलेशन की शुरुआत दोबारा 1950 के दशक में हुई थी जब मैकी जी ने पहले बार प्रोग्राम कोम्प्यूटर सिंथेसाइज़र दिया था। उस दौरान, स्ट्रिंग इंटरपोलेशन का उपयोग सिंथेसाइज़र द्वारा दिए गए संगीतीय धुनों को नोट चार्ट्स पर छपने के लिए किया जाता था।
(2) अन्य विकल्पों में स्ट्रिंग फॉर्मेटिंग और स्ट्रिंग बिल्डर हैं।
(3) जावा में, स्ट्रिंग इंटरपोलेशन को स्ट्रिंग बिल्डर के रूप में फाइनल कीवर्ड के साथ समर्थित किया गया है। इससे स्ट्रिंग ट्रेंच इंटरपोलेटर, स्ट्रिंग इंटरपोलेशन का अन्य एक रूप, उपयोगकर्ताओं को डायनामिक टेम्पलेट स्ट्रिंग बनाने की अनुमति देता है।

## भी देखें

- [स्ट्रिंग इंटरपोलेशन का और गहराई में जानकारी](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [स्ट्रिंग बिल्डर](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)