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

## क्यों

क्या आपने कभी किसी एप्लिकेशन या वेबसाइट पर डेटा को उपलब्ध कराने के लिए JSON या "जेस्न" को सुना है? हां, ऐसा होता है क्योंकि यह एक पॉपुलर डेटा फॉर्मेट है जो विभिन्न एप्लिकेशन और सर्विसेज के बीच डेटा को सरल रूप से साझा करने का एक आसान तरीका है। JSON को समझना और उसके साथ काम करना कोई कठिन काम नहीं है और इसके लिए Kotlin भी एक मजबूत भाषा है। इस लेख में, हम देखेंगे कि Kotlin में JSON को कैसे उपयोग किया जा सकता है और इसे समझने के लिए कुछ गहराई में भी जाएंगे।

## कैसे करें

एक साधारण JSON फ़ाइल को Kotlin में कैसे पढ़ा जाए, बस कुछ ही लाइनों में आप देख सकते हैं:

```Kotlin
val jsonString = """ 
{
    "नाम": "क्या पता",
    "उम्र": 28,
    "शहर": "दिल्ली"
} 
"""

val jsonObject = JSONObject(jsonString)
println("नाम: " + jsonObject.getString("नाम"))
println("उम्र: " + jsonObject.getInt("उम्र"))
println("शहर: " + jsonObject.getString("शहर"))
```

पहले हमने Kotlin में एक स्ट्रिंग से बनी JSON फ़ाइल बनाई है और उसे `jsonString` नाम के वेरिएबल में स्टोर किया है। फिर हमने इस `jsonString` को `JSONObject` कॉन्स्ट्रक्टर के साथ डाला है, जो एक JSONObject के ऑब्जेक्ट को बनाता है। फाइनली, हमने `getString` और `getInt` के माध्यम से अलग-अलग फ़ील्ड्स की वैल्यू निकाली है और प्रिंट किया है।

अगर आपको JSON फ़ाइल में अन्य तरह की डेटा है, जैसे कि एक एरे, तो उसे Kotlin के साथ कैसे पढ़ा जाए, देखें:

```Kotlin
val jsonArrayString = """ 
[
    {
        "उपनाम": "आ