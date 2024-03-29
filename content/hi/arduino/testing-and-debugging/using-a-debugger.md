---
date: 2024-01-26 03:48:59.225087-07:00
description: "\u0921\u093F\u092C\u0917\u0930 \u090F\u0915 \u0909\u092A\u0915\u0930\
  \u0923 \u0939\u0948 \u091C\u094B \u0906\u092A\u0915\u0947 \u0915\u094B\u0921 \u092E\
  \u0947\u0902 \u092C\u0917\u094D\u0938 \u0915\u094B \u0928\u093F\u0915\u093E\u0932\
  \u0928\u0947 \u092E\u0947\u0902 \u092E\u0926\u0926 \u0915\u0930\u0924\u093E \u0939\
  \u0948 \u0924\u093E\u0915\u093F \u0906\u092A \u0930\u0941\u0915 \u0938\u0915\u0947\
  \u0902, \u0906\u0938-\u092A\u093E\u0938 \u0926\u0947\u0916 \u0938\u0915\u0947\u0902\
  , \u0914\u0930 \u0938\u092E\u091D \u0938\u0915\u0947\u0902 \u0915\u093F \u0905\u0938\
  \u0932 \u092E\u0947\u0902 \u0928\u0940\u091A\u0947 \u0915\u094D\u092F\u093E \u0939\
  \u094B \u0930\u0939\u093E \u0939\u0948\u0964\u2026"
lastmod: '2024-03-13T22:44:52.781222-06:00'
model: gpt-4-0125-preview
summary: "\u0921\u093F\u092C\u0917\u0930 \u090F\u0915 \u0909\u092A\u0915\u0930\u0923\
  \ \u0939\u0948 \u091C\u094B \u0906\u092A\u0915\u0947 \u0915\u094B\u0921 \u092E\u0947\
  \u0902 \u092C\u0917\u094D\u0938 \u0915\u094B \u0928\u093F\u0915\u093E\u0932\u0928\
  \u0947 \u092E\u0947\u0902 \u092E\u0926\u0926 \u0915\u0930\u0924\u093E \u0939\u0948\
  \ \u0924\u093E\u0915\u093F \u0906\u092A \u0930\u0941\u0915 \u0938\u0915\u0947\u0902\
  , \u0906\u0938-\u092A\u093E\u0938 \u0926\u0947\u0916 \u0938\u0915\u0947\u0902, \u0914\
  \u0930 \u0938\u092E\u091D \u0938\u0915\u0947\u0902 \u0915\u093F \u0905\u0938\u0932\
  \ \u092E\u0947\u0902 \u0928\u0940\u091A\u0947 \u0915\u094D\u092F\u093E \u0939\u094B\
  \ \u0930\u0939\u093E \u0939\u0948\u0964\u2026"
title: "\u0921\u0940\u092C\u0917\u0930 \u0915\u093E \u0909\u092A\u092F\u094B\u0917"
---

{{< edit_this_page >}}

## क्या और क्यों?

डिबगर एक उपकरण है जो आपके कोड में बग्स को निकालने में मदद करता है ताकि आप रुक सकें, आस-पास देख सकें, और समझ सकें कि असल में नीचे क्या हो रहा है। प्रोग्रामर्स अपने कोड के माध्यम से जाने, वेरिएबल्स की जांच करने, और समझने के लिए कि चीजें कहाँ गलत हो सकती हैं, के लिए डिबगर्स का उपयोग करते हैं।

## कैसे:

Arduino IDE के साथ, आप डिबगिंग के लिए Serial प्रिंट्स का उपयोग कर सकते हैं, लेकिन यह एक गुफा का पता लगाने के लिए टॉर्च का उपयोग करने जैसा है। वास्तविक डिबगिंग के लिए, आप अपनी खेल को Atmel-ICE डिबगर जैसी किसी चीज़ के साथ बढ़ाना चाह सकते हैं जो Arduino वातावरण के साथ एकीबद्ध होता है। यहाँ Serial का उपयोग करके प्सूडो-डिबगिंग का एक नमूना है:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("सेंसर मूल्य: ");
  Serial.println(sensorValue);
  // मान लीजिए आप यहाँ 512 की उम्मीद कर रहे हैं, लेकिन 0 प्राप्त करें।
  // सेंसर कनेक्शन की जांच करने का समय
  delay(1000); // फिर से पढ़ने से पहले एक सेकंड के लिए इंतजार करो
}
```
इसे Serial Monitor को खोलकर चलाएं, और आप देख सकते हैं कि आपका सेंसर वास्तविक समय में क्या निकाल रहा है।

## गहन विश्लेषण

डिबगर्स से पहले, यह प्रिंट स्टेटमेंट की दुनिया थी – आप सबकुछ प्रिंट करके केवल अनुमान लगा सकते थे कि क्या हो रहा था। प्रिंट्स का उपयोग करके डिबगिंग अब भी आम है, विशेष रूप से सरल वातावरणों में या जैसे Arduino जैसे संसाधन सीमित हार्डवेयर पर।

Atmel-ICE जैसे इन-सर्किट एम्यूलेटर्स के विकल्पों में सॉफ़्टवेयर डिबगिंग उपकरण जैसे `avr-gdb` शामिल हैं। आप इसे `avarice` के साथ जोड़ सकते हैं ताकि GDB और आपके हार्डवेयर के बीच एक पुल बना सकें, जो चिप पर और अधिक उन्नत डिबगिंग के लिए बेहद उपयोगी है।

एक डिबगर का उपयोग करके, आप कुछ बिंदुओं पर क्रियान्वयन को रोकने के लिए ब्रेकपॉइंट्स सेट कर सकते हैं। आप अपने कोड को लाइन दर लाइन आगे बढ़ा सकते हैं, स्मृति, रजिस्टर्स, और वेरिएबल्स की जांच कर सकते हैं। यह आपको अंधेरे में शॉट्स लेने की बजाय समस्याओं को पिनपॉइंट करने देता है। जब आप एक डिबगर लागू कर रहे हों, तो सुनिश्चित करें कि आपका वातावरण सही ढंग से सेट है - असंगत संस्करण या खराब कॉन्फ़िगर किए गए उपकरण निराशा का कारण बन सकते हैं।

## देखें भी

गहराई में जाने के लिए तैयार हैं? इनमें डाइव करें:
- [Arduino डिबगिंग](https://www.arduino.cc/en/Guide/Environment#toc7) पर Arduino डिबगिंग गाइड
- avr-gdb सेट अप करने के लिए AVR Libc संदर्भ मैनुअल: [AVR Libc होम पेज](http://www.nongnu.org/avr-libc/)
