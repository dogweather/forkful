---
title:                "नई परियोजना शुरू करना"
aliases:
- /hi/arduino/starting-a-new-project.md
date:                  2024-01-20T18:03:14.237303-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
नया प्रोजेक्ट शुरु करना मतलब आपके आइडिया को एक ठोस आकार देना है। प्रोग्रामर नए प्रोजेक्ट इसलिए शुरू करते हैं क्योंकि यह नवाचार, शिक्षा, और समस्याओं के समाधान का माध्यम बनता है।

## कैसे करें:
चलिए, बातें कम करते हैं और कोडिंग शुरू करते हैं। साधारण LED ब्लिंक प्रोग्राम देखते हैं:

```Arduino
void setup() {
  pinMode(13, OUTPUT); // LED को पिन नंबर 13 से जोड़ें
}

void loop() {
  digitalWrite(13, HIGH);   // LED चालू करें
  delay(1000);              // 1 सेकंड के लिए रुकें
  digitalWrite(13, LOW);    // LED बंद करें
  delay(1000);              // 1 सेकंड के लिए रुकें
}
```

आउटपुट: LED हर 1 सेकंड बाद ब्लिंक करेगी।

## गहराई में:
Arduino प्रोजेक्ट शुरू करने का इतिहास २००५ में पड़ता है, जब ये इटली में छात्रों को सीखने के लिए बनाया गया था। इसके विकल्पों में Raspberry Pi और BeagleBone जैसे माइक्रोकंट्रोलर्स शामिल हैं। एक Arduino प्रोजेक्ट शुरू करते समय, पहचानें कि क्या आपको अतिरिक्त लाइब्रेरीज या मॉड्यूल्स की आवश्यकता है। अपनी प्रोजेक्ट को मॉड्यूलर बनाने की योजना बनाएं ताकि उसे सुधारना और विस्तार करना आसान हो।

## और भी देखें:
- Arduino की आधिकारिक वेबसाइट: [arduino.cc](https://www.arduino.cc/)
- Arduino फोरम, समुदाय से मदद और सलाह के लिए: [forum.arduino.cc](https://forum.arduino.cc/)
- Arduino प्रोजेक्ट्स के लिए ट्यूटोरियल्स और कोर्सेज: [arduino.cc/en/Tutorial/HomePage](https://www.arduino.cc/en/Tutorial/HomePage)
