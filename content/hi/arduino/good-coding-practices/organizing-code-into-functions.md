---
date: 2024-01-26 01:10:35.922453-07:00
description: "\u0915\u094B\u0921 \u0915\u094B \u092B\u0902\u0915\u094D\u0936\u0928\
  \u094D\u0938 \u092E\u0947\u0902 \u0906\u092F\u094B\u091C\u093F\u0924 \u0915\u0930\
  \u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0905\u092A\u0928\
  \u0947 \u0915\u094B\u0921 \u0915\u094B \u092A\u0941\u0928: \u092A\u094D\u0930\u092F\
  \u094B\u091C\u094D\u092F \u0916\u0902\u0921\u094B\u0902 \u092E\u0947\u0902 \u0935\
  \u093F\u092D\u093E\u091C\u093F\u0924 \u0915\u0930\u0928\u093E, \u092A\u094D\u0930\
  \u0924\u094D\u092F\u0947\u0915 \u0916\u0902\u0921 \u090F\u0915 \u0935\u093F\u0936\
  \u093F\u0937\u094D\u091F \u0915\u093E\u0930\u094D\u092F \u0915\u0930 \u0930\u0939\
  \u093E \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947\u2026"
lastmod: '2024-03-13T22:44:52.782955-06:00'
model: gpt-4-1106-preview
summary: "\u0915\u094B\u0921 \u0915\u094B \u092B\u0902\u0915\u094D\u0936\u0928\u094D\
  \u0938 \u092E\u0947\u0902 \u0906\u092F\u094B\u091C\u093F\u0924 \u0915\u0930\u0928\
  \u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0905\u092A\u0928\u0947\
  \ \u0915\u094B\u0921 \u0915\u094B \u092A\u0941\u0928: \u092A\u094D\u0930\u092F\u094B\
  \u091C\u094D\u092F \u0916\u0902\u0921\u094B\u0902 \u092E\u0947\u0902 \u0935\u093F\
  \u092D\u093E\u091C\u093F\u0924 \u0915\u0930\u0928\u093E, \u092A\u094D\u0930\u0924\
  \u094D\u092F\u0947\u0915 \u0916\u0902\u0921 \u090F\u0915 \u0935\u093F\u0936\u093F\
  \u0937\u094D\u091F \u0915\u093E\u0930\u094D\u092F \u0915\u0930 \u0930\u0939\u093E\
  \ \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930 \u0907\u0938\u0947\u2026"
title: "\u0915\u094B\u0921 \u0915\u094B \u092B\u0902\u0915\u094D\u0936\u0928\u094D\
  \u0938 \u092E\u0947\u0902 \u0935\u094D\u092F\u0935\u0938\u094D\u0925\u093F\u0924\
  \ \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोड को फंक्शन्स में आयोजित करने का मतलब है अपने कोड को पुन: प्रयोज्य खंडों में विभाजित करना, प्रत्येक खंड एक विशिष्ट कार्य कर रहा होता है। प्रोग्रामर इसे कोड को अधिक पढ़ने योग्य, डीबग करने में आसान और पुन: उपयोग करने के लिए करते हैं। यह लेगोस को डिब्बों में छांटने जैसा है - यह आपको हर बार जब आप कुछ बनाना चाहते हैं, तो एक अस्त-व्यस्त ढेर से खोजबीन करने से बचाता है।

## कैसे करें:
कल्पना करें कि आप एक LED को ब्लिंक करना चाहते हैं। फंक्शन्स के बिना, आपका `loop` एक अव्यवस्थित उलझन होती है। फंक्शन्स के साथ, यह सुव्यवस्थित है। ये रहा कैसे:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // हर 500 मिलीसेकंड बाद LED को ब्लिंक करें
}

// LED को ब्लिंक करने के लिए फंक्शन
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

नमूना आउटपुट: आपकी LED खुशी से झपक रही है, और कोड का उद्देश्य एक नजर में स्पष्ट है।

## गहराई से विचार
फंक्शन्स से पहले, प्रोग्रामिंग एक रेखीय रोड ट्रिप की तरह थी; आप हर गड्ढे को शुरू से अंत तक देख सकते थे। फंक्शन्स के बाद, यह उड़ानों को होप करने की तरह है - आप महत्वपूर्ण हिस्सों पर सीधे जा सकते हैं। ऐतिहासिक रूप से, सबरूटीन्स (प्रारंभिक फंक्शन्स) प्रोग्रामिंग में एक क्रांति थे, जिसने कोडर्स को स्वयं को दोहराने से बचने दिया – वह है DRY सिद्धांत, डोंट रिपीट योरसेल्फ। फंक्शन्स के विकल्पों में मैक्रोस या ऑब्जेक्ट-ओरिएंटेड प्रोग्रामिंग (OOP) के लिए क्लासेस का उपयोग शामिल हो सकता है। बारीकी से? जब आप एक फंक्शन को परिभाषित करते हैं, तो आप कम्पाइलर को एक कार्य को करने के लिए एक ब्लूप्रिंट दे रहे हैं। Arduino के साथ, आप अक्सर वीड फंक्शन्स को परिभाषित कर रहे हैं जो माइक्रोकंट्रोलर के लिए साधारण आदेशों की तरह कार्य करते हैं, लेकिन फंक्शन्स मान वापस कर सकते हैं, जिससे वे अधिक बहुमुखी बन जाते हैं।

## देखें भी
फंक्शन्स के बारे में और जानकारी के लिए, इन्हें ब्राउज़ करें:

- Arduino का आधिकारिक फंक्शन संदर्भ: https://www.arduino.cc/reference/en/language/functions/
- DRY सिद्धांत के बारे में और जानें: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- सबरूटीन्स के इतिहास पर एक रिफ्रेशर: https://en.wikipedia.org/wiki/Subroutine
