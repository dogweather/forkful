---
title:                "स्ट्रिंग से दिनांक पार्स करना"
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# क्या और क्यों?
डेटा को स्ट्रिंग से पारस करना क्या है और क्यों क्रमश: प्रोग्रामर इसे करते हैं।
डेटा को स्ट्रिंग से पारस करना काम आपको अपने कोड में अतिरिक्त जानकारी प्रदान करता है, जो आसानी से उपयोग के लिए भेंट देता है। यह आपको डेटा के भिन्न एक अंश या महत्वपूर्ण विवरण को अलग करने की अनुमति देता है।

## कैसे करें:
```arduino
String date = "10/29/2021"; // स्ट्रिंग विकल्प बनाएँ।

// डेटा को पारस करने के लिए आप डेटा स्ट्रिंग से अलग कर सकते हैं और उसे संग्रहित कर सकते हैं।
int day = date.substring(3,5);
int month = date.substring(0,2);
int year = date.substring(6,10);

// संग्रहित डेटा को उपयोग करें
Serial.print("आज की तारीख: ");
Serial.print(month);
Serial.print("/");
Serial.print(day);
Serial.print("/");
Serial.println(year);
```

उदाहरण में, "date" स्ट्रिंग में दिए गए तिथि को 10/29/2021 के समान अनुभव करने के लिए स्पलिट किया जाता है। संग्रहित तिथि को सीरियल मॉनिटर के माध्यम से प्रिंट किया गया है।

## गहराई में जाएं:
पहले, स्थानिक समय प्राप्त करने के लिए वैक्टर्य में दिनांक हो सकता है। प्रोग्रामरों को आमतौर पर स्थानिक समय एवं तारीख को एक स्थिति में बदलना चाहिए, के अपेक्षाकृत सबसे अच्छे रूप से इस तारीख को पारस करना होगा। एक दिन को अनुकूलित करने के लिए सलाह है, कदाचलेन, स्थानिक समय के अंतर के कारण संरक्षण किए जोड़ सकते हैं।

संबद्ध, यदि श्याखा पर संरक्षित डेट येज अर्थपुर्ण समय की अग्रेषण को प्रकट करें, तो स्वत ही प्रसंस्करण करते हुए प्रसंस्करण कर सकते हो। उदाहरण के लिए, अगर आप कॉन्स्ट्रक्टर को 1970 में डालोगे, तो आप 1970 में कॉन्सड ना समकलकि नापिचा करने के देश सत्तथि ओ आ विभात क्रमों किलता है| यद्यपि, समय के कोड जो करने सापिर को ।

## भी देखें:
संग्रहित डेटा तक पहुचने के लिए इस ट्यूटोरियल के इस स्रोत को देखें। यह आर्दुनो कॉम्यूनिटी के माध्यम से उपलभ्ध है। यह आपको स्थानिक समय से संबद्ध सभी जानकारी देता है और आपको संग्रहित तिथि को संस्कृत में पारस करने के लिए तैयार करता