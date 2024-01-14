---
title:                "Java: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON (JavaScript Object Notation) डेटा को एक आसान और व्यापक तरीके से संग्रहीत करने के लिए एक बहुत ही प्रभावी प्रोग्रामिंग भाषा है। यह सुसंगत है जब आप विभिन्न स्थानों से अलग-अलग प्रकार की डेटा में आसानी से पहुंच करने की आवश्यकता हो। अपनी मुख्यता से सुविधाओं के कारण, यह एक अच्छा विकल्प है और जावा प्रोग्रामर्स के लिए एक महत्वपूर्ण टूल है।

## कैसे करें

JSON का उपयोग करके डेटा परिवर्तित करने के लिए, आपको कुछ सामान्य चरणों को पालन करने की आवश्यकता होगी। सबसे पहले, आपको जावा में "JSONObject" और "JSONArray" के इस्तेमाल से अपने कोड में इनपुट और आउटपुट मॉडल को परिभाषित करना होगा।
आप इस लेख के अंत में विस्तृत जावा कोड का उदाहरण देख सकते हैं।

जब आपका मॉडल तैयार हो, तो आप उसके साथ काम कर सकते हैं। आप "JSONObject" का उपयोग करके अपने इनपुट स्ट्रिंग को अपने मॉडल में पारस्परिक रूप से पेश कर सकते हैं। आप "JSONArray" का उपयोग करके अपने मॉडल से डेटा को पढ़ और बनाएं और और उसे फाइल या अन्य संरचनाओं में लिख सकते हैं। आप अपनी पसंद के अनुसार इन तरीकों का उपयोग करके डेटा को संगठित कर सकते हैं।

```Java
// डेटा के लिए मॉडल तैयार करें
JSONObject employee = new JSONObject();
employee.put("name", "राम");
employee.put("age", 25);

JSONArray hobbies = new JSONArray();
hobbies.put("क्रिकेट");
hobbies.put("फुटबॉल");

employee.put("hobbies", hobbies);

System.out.println