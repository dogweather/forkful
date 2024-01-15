---
title:                "json के साथ काम करना"
html_title:           "TypeScript: json के साथ काम करना"
simple_title:         "json के साथ काम करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

जेसोन (JSON) प्रोग्रामिंग दुनिया में बहुत अहम है। यह एक लोकप्रिय डेटा प्रारूप है जो विभिन्न एप्लिकेशनों, साइटों और सिस्टमों में डेटा को संगठित करने के लिए इस्तेमाल किया जाता है। इसलिए, जेसोन के साथ काम करना हर प्रोग्रामर के लिए आवश्यक है।

## कैसे करें

जेसोन को टाइपस्क्रिप्ट (TypeScript) में इस्तेमाल करना बहुत आसान है। आप `JSON.parse()` और `JSON.stringify()` फंक्शन को इस्तेमाल करके जेसोन डेटा को आसानी से पारसी कर सकते हैं। निम्नलिखित उदाहरण आपको समझने में मदद करेंगे: 

```TypeScript
// एक जेसोन डेटा बनाएं
let data = {
    name: "John",
    age: 25
};

// डेटा को स्ट्रिंग में परिवर्तित करें
let stringData = JSON.stringify(data);
console.log(stringData); // आउटपुट: {"name":"John","age":25}

// स्ट्रिंग को जेसोन डेटा में परिवर्तित करें
let parsedData = JSON.parse(stringData);
console.log(parsedData); // आउटपुट: { name: "John", age: 25 }
```

## गहराई में जाएं

जेसोन एक सरल डेटा प्रारूप है लेकिन उम्ब्रे तारीकों से उसके साथ काम करने के लिए कई अन्य विकल्प हैं। आप `JSON.stringify()` में एक तीसरा पैरामीटर पारसी फंक्शन के रूप में पास करके विकल्पों का उपयोग कर सकते हैं। यह आपको जेसोन डेटा को अपने अनुमानित प्रारूप में प्रदर्शित करने में मदद कर सकता है। आप `JSON.parse()` के लिए भी एक दूसरा पैरामीटर उपयोग करके अपनी सामान्य विकल्पों को सेट कर सकते हैं। जेसोन से सम्बन्धित और गहराइयों क