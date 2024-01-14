---
title:                "Kotlin: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों
जेसोन(JSON) के साथ काम करने में कोई संबंध क्यों बनाएं?

## कैसे करें
`Kotlin` का सरावण करते समय जेसोन(JSON) का उपयोग कैसे करें:

जेसोन(JSON) एक संग्रहीत डाटा प्रारूप है जो कि कुछ आसान `key-value` पैरों से मिलकर बनता है। यह डाटा को अन्य साधनों जैसे डेटाबेस, वेब सर्वर आदि से आसानी से भेजने और प्राप्त करने को सुनिश्चित करता है। जेसेन(JSON) को संग्रहीत करना और पार्स करना भी बहुत आसान होता है। नीचे दिए गए कोड ब्लॉक्स में उदाहरण है:

```Kotlin
// JSON फ़ाइल बनाएं
val jsonObject = JSONObject()
jsonObject.put("name", "John")
jsonObject.put("age", 30)
jsonObject.put("hobby", "playing guitar")

// JSON स्ट्रिंग को प्रिंट करें
println(jsonObject.toString())

// Output: {"name":"John","age":30,"hobby":"playing guitar"}

// JSON स्ट्रिंग को पार्स करें
val jsonString = "{\"name\":\"Jane\",\"age\":25,\"hobby\":\"reading books\"}"
val json = JSONObject(jsonString)

// डेटा प्रिंट करें
println(json.getString("name"))
println(json.getInt("age"))
println(json.getString("hobby"))

// Output: Jane
//         25
//         reading books
```

## गहराई में जाएं
जेसोन(JSON) के साथ काम करते समय और उसके अंदर हुए बदलावों को समझने के लिए अधिक जानकारी के लिए आप निम्नलिखित लिंक्स पर जा सकते हैं:

- [Kotlin JSON डॉक्यूमेंटेशन](https://kotlinlang.org/docs/reference/js-interop.html)
- [जेसोन(JSON) का प्रारूप](https://www.json.org/json-en.html)
- [जावा स्टैंडर्ड JSON लाइब्रेरी](https://github.com/stleary/JSON-java) 

## और भी देखें
अन्य साधनों को सीखने के लिए आप इसे लिंक्स पर जान सकते हैं:

- [Kotlin आधिकारिक वेबसाइट](https://kotlinlang.org/)
- [Kotlin कोडिंग प्रोजेक्ट्स](https://github.com/KotlinBy/awesome-kotlin)