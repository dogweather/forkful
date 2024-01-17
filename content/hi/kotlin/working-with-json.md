---
title:                "Json के साथ काम करना"
html_title:           "Kotlin: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON काफी सरल और प्रभावी ढंग से डेटा को संसाधित करने और संचित करने के लिए इस्तेमाल किया जाता है। प्रोग्रामरों के लिए, JSON डेटा को पढ़ने और लिखने के लिए सुविधाजनक समाधान प्रदान करता है।

## कैसे?

JSON का उपयोग Kotlin में खासकर ```JSONObject``` और ```JSONArray``` कक्षाओं का उपयोग करके किया जा सकता है। यहां हम दो कदमों में आपको समझाएंगे कि कैसे एक JSON अभिलेख को पढ़कर उसके लिए आवश्यक डेटा को निकाला जाता है।

#### कदम 1: एक नया JSON अभिलेख बनाएं

```Kotlin
val jsonObject = JSONObject()
jsonObject.put("name", "John")
jsonObject.put("age", 25)
```

#### कदम 2: डेटा पढ़ें और उसका उपयोग करें

```Kotlin
val name = jsonObject.getString("name")
val age = jsonObject.getInt("age")
println("Name: $name, Age: $age")
```

आउटपुट:
```
Name: John, Age: 25
```

## गहराई में जाएं

### ऐतिहासिक संदर्भ

JavaScript Object Notation (JSON) को माइक्रोसॉफ्ट के Doug Crockford ने 1999 में बनाया था। यह XML के समान रूप से डेटा को संरचित करने के लिए बनाया गया था, लेकिन यह कम अंतरिक्ष ले तो, मजबूत, सरल और शीघ्र पारित होने के लिए जाना जाता है। आजकल, JSON को कई भाषाओं में समर्थित किया जाता है, सहित Kotlin।

### विकल्प

XML, CSV और YAML ये सभी एकसाथ जुड़े-समझे डेटा को संसाधित करने के लिए अलग-अलग फॉर्मेट हैं। हालांकि, JSON एक उत्तम विकल्प है क्योंकि यह सरल, पारदर्शी और स्थापित होने के कारण पसंद किया जाता है।

### अंतर्दृष्टि की विविधता

Kotlin में JSON डेटा का प्रबंधन करने के लिए दो विकल्प हैं - ```JSONObject``` और ```JSONArray```। ```JSONObject``` किसी कुंजी (key) और मान (value) के का एक समूह है, जबकि ```JSONArray``` कुंजी विशिष्ट नहीं है। इन दोनों में से आप अपनी आवश्यकतानुसार चुन सकते हैं।

## और देखें

- Kotlin के लिए JSON से संबंधित अधिक जानकारी के लिए [यह](https://kotlinlang.org/docs/reference/js-interop.html#working-with-json) देखें।
- JSON को पढ़ने और लिखने के लिए सुविधाजनक Kotlin लाइब्रेरी के लिए [यह](https://github.com/Kotlin/ktor) देखें।