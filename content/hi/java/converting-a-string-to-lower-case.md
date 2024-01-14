---
title:                "Java: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों
जब हम किसी स्ट्रिंग को लोअर केस में बदलने का विचार करते हैं, तो ध्यान देने वाली एक बहुत आम वजह है कि हमें स्ट्रिंग को दूसरे स्ट्रिंग के साथ संबंधित करना हो और दोनों स्ट्रिंग को एक समान रूप से तुलना करना हो। इसलिए, स्ट्रिंग को लोअर केस में बदलने से हम उन्हें एक समान तुलना कर सकते हैं और उनका एक ही रूप बना सकते हैं।

## कैसे करें
```Java
String sampleString = "Hello World";
String lowerCaseString = sampleString.toLowerCase(); //output: hello world
```

स्ट्रिंग को लोअर केस में बदलने के लिए, हम `toLowerCase()` मेथड का उपयोग कर सकते हैं जो String आइटम को लोअर केस में बदल देता है। यदि हम किसी भी स्ट्रिंग पर इस मेथड को लागू करते हैं तो वह स्वचालित रूप से उस स्ट्रिंग को लोअर केस में बदल देगा।

```Java
String empName = "Ajeet Kumar";
if(empName.toLowerCase().equals("ajeet kumar")) { //output: true
    System.out.println("Employee name match found");
}
```

इस उदाहरण में, हम `toLowerCase()` मेथड को स्ट्रिंग को संशोधित करने के लिए उपयोग करने के स्थान पर चेकिंग के लिए उपयोग करते हैं। यह उपयोगकर्ता के द्वारा प्रदान किए गए प्रविष्ट किए गए नाम के साथ स्ट्रिंग में मेल खोजता है और अगर मेल मिलता है तो सही होगा अन्यथा गलत होगा।

## गहराई की तरफ
स्ट्रिंग को लोअर केस में बदलने का एक और तरीका है `String.toLowerCase(Locale loc)` मेथड का उपयोग करके जो किसी निर्दिष्ट टिप्पणी (लोकेल) के अनुसार स्ट्रिंग को लोअर केस में बद