---
title:                "यामल के साथ काम करना"
html_title:           "Java: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों?
 YAML काम करने का एक साधन है जिसके माध्यम से डेटा को संगठित रूप में रखा जा सकता है। यह एक आसान और आरामदायक तरीके से डेटा को तरल रूप में संगठित करता है। यह प्रोग्रामरों को इस्तेमाल करने के लिए करता है क्योंकि इससे उन्हें कोड को हार्ड कोड करने की जरूरत नहीं होती है और प्रोग्राम को बदलने के लिए तुरंत प्रतिक्रिया मिलती है।

## कैसे करें:
 ```java
 import org.yaml.snakeyaml.Yaml;

 // डेटा बनाएं
 Map<String, Object> data = new HashMap<>();
 data.put("name", "जॉन डो"); 
 data.put("age", 25); 
 data.put("address", "न्यूयॉर्क");
 
 // यामल ऑब्जेक्ट बनाएं 
 Yaml yaml = new Yaml(); 
 String output = yaml.dump(data); 
 System.out.println(output); 

 // अब उपयोग करें और डालें 
 Map<String, Object> parsed = (Map<String, Object>) yaml.load(output); 
 System.out.println(parsed.get("name")); 
 System.out.println(parsed.get("age")); 
 System.out.println(parsed.get("address"));
 ```
 आउटपुट:
 ```
 name: जॉन डो
 age: 25
 address: न्यूयॉर्क
 
 जॉन डो 
 25
 न्यूयॉर्क
 ```
 
## गहराई में जाएं:
 YAML की शुरुआत 2001 में Dan Karmett द्वारा हुई थी जो Perl में लिखे गए थे। इसके अलावा, दुसरे प्रोग्रामिंग भाषाओं में भी YAML उपलब्ध है। YAML का अविकल्पिक प्रतिदोर्वार जूलियो विदियो ने भी अपने प्रोग्रामिंग भाषा जूलियो के लिए बनाया था। YAML को अन्य सीमाएं और अनुबंधों के भी रूप में भी उपयोग किया जाता है। यह कुछ अलग हो सकता है लेकिन इसे पहचानने और समझने में आसान है।

## इसके साथ देखें:
- [YAML आधिकारिक वेबसाइट](https://yaml.org/)
- [यामल विकि](https://en.wikipedia.org/wiki/YAML)
- [JYAML - Java के लिए YAML इम्प्लीमेंटेशन](https://code.google.com/archive/p/jyaml/)