---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:38.232073-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Arduino \u092E\u0942\
  \u0932\u0924\u0903 \u092E\u093E\u0928\u0915 \u0906\u0909\u091F\u092A\u0941\u091F\
  \ \u0914\u0930 \u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\
  \u0947 \u092C\u0940\u091A \u0905\u0902\u0924\u0930 \u0928\u0939\u0940\u0902 \u0915\
  \u0930\u0924\u093E, \u091C\u0948\u0938\u093E \u0915\u093F \u092A\u093E\u0930\u0902\
  \u092A\u0930\u093F\u0915 \u0915\u0902\u092A\u094D\u092F\u0942\u091F\u093F\u0902\u0917\
  \ \u0938\u093F\u0938\u094D\u091F\u092E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\
  \u0964 `Serial.print()` \u0914\u0930\u2026"
lastmod: '2024-04-05T21:53:54.764861-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u092E\u0942\u0932\u0924\u0903 \u092E\u093E\u0928\u0915 \u0906\u0909\
  \u091F\u092A\u0941\u091F \u0914\u0930 \u092E\u093E\u0928\u0915 \u0924\u094D\u0930\
  \u0941\u091F\u093F \u0915\u0947 \u092C\u0940\u091A \u0905\u0902\u0924\u0930 \u0928\
  \u0939\u0940\u0902 \u0915\u0930\u0924\u093E, \u091C\u0948\u0938\u093E \u0915\u093F\
  \ \u092A\u093E\u0930\u0902\u092A\u0930\u093F\u0915 \u0915\u0902\u092A\u094D\u092F\
  \u0942\u091F\u093F\u0902\u0917 \u0938\u093F\u0938\u094D\u091F\u092E \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902\u0964 `Serial.print()` \u0914\u0930 `Serial.println()`\
  \ \u0926\u094B\u0928\u094B\u0902 \u0935\u093F\u0927\u093F\u092F\u093E\u0902 \u0938\
  \u092E\u093E\u0928 \u0938\u0940\u0930\u093F\u092F\u0932 \u0906\u0909\u091F\u092A\
  \u0941\u091F \u092A\u0930 \u0932\u093F\u0916\u0924\u0940 \u0939\u0948\u0902, \u091C\
  \u093F\u0938\u0947 \u0906\u092E\u0924\u094C\u0930 \u092A\u0930 Arduino IDE \u0938\
  \u0940\u0930\u093F\u092F\u0932 \u092E\u0949\u0928\u093F\u091F\u0930 \u092E\u0947\
  \u0902 \u0926\u0947\u0916\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0939\
  \u093E\u0932\u093E\u0902\u0915\u093F, \u0939\u092E \u0924\u094D\u0930\u0941\u091F\
  \u093F \u0938\u0902\u0926\u0947\u0936\u094B\u0902 \u0915\u094B \u0935\u093F\u0936\
  \u0947\u0937 \u0930\u0942\u092A \u0938\u0947 \u092B\u0949\u0930\u094D\u092E\u0947\
  \u091F \u0915\u0930\u0915\u0947 \u092F\u093E \u0909\u0928\u094D\u0939\u0947\u0902\
  \ \u090F\u0915 \u0935\u0948\u0915\u0932\u094D\u092A\u093F\u0915 \u0906\u0909\u091F\
  \u092A\u0941\u091F \u092A\u0930 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\
  \u0924 \u0915\u0930\u0915\u0947 stderr \u0915\u0947 \u0932\u093F\u090F \u0932\u0947\
  \u0916\u0928 \u0915\u093E \u0905\u0928\u0941\u0915\u0930\u0923 \u0915\u0930 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902, \u091C\u0948\u0938\u0947 \u0915\u093F \u090F\
  \u0938\u0921\u0940 \u0915\u093E\u0930\u094D\u0921 \u092A\u0930 \u090F\u0915 \u092B\
  \u093E\u0907\u0932 \u092F\u093E \u0928\u0947\u091F\u0935\u0930\u094D\u0915 \u0915\
  \u0928\u0947\u0915\u094D\u0936\u0928 \u0915\u0947 \u091C\u0930\u093F\u090F\u0964\
  \ stderr \u0915\u093E \u0905\u0928\u0941\u0915\u0930\u0923 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A \u0938\u0940\u0930\u093F\u092F\u0932\
  \ \u092E\u0949\u0928\u093F\u091F\u0930 \u092E\u0947\u0902 \u0924\u094D\u0930\u0941\
  \u091F\u093F \u0938\u0902\u0926\u0947\u0936\u094B\u0902 \u0915\u094B \u0905\u0932\
  \u0917 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \"\u0924\u094D\u0930\
  \u0941\u091F\u093F:\" \u091C\u0948\u0938\u0947 \u091F\u0948\u0917 \u0915\u0947 \u0938\
  \u093E\u0925 \u0924\u094D\u0930\u0941\u091F\u093F \u0938\u0902\u0926\u0947\u0936\
  \u094B\u0902 \u0915\u094B \u092A\u0942\u0930\u094D\u0935\u0935\u0930\u094D\u0924\
  \u0940 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे करें:
Arduino मूलतः मानक आउटपुट और मानक त्रुटि के बीच अंतर नहीं करता, जैसा कि पारंपरिक कंप्यूटिंग सिस्टम करते हैं। `Serial.print()` और `Serial.println()` दोनों विधियां समान सीरियल आउटपुट पर लिखती हैं, जिसे आमतौर पर Arduino IDE सीरियल मॉनिटर में देखा जाता है। हालांकि, हम त्रुटि संदेशों को विशेष रूप से फॉर्मेट करके या उन्हें एक वैकल्पिक आउटपुट पर निर्देशित करके stderr के लिए लेखन का अनुकरण कर सकते हैं, जैसे कि एसडी कार्ड पर एक फाइल या नेटवर्क कनेक्शन के जरिए।

stderr का अनुकरण करने के लिए, आप सीरियल मॉनिटर में त्रुटि संदेशों को अलग करने के लिए "त्रुटि:" जैसे टैग के साथ त्रुटि संदेशों को पूर्ववर्ती कर सकते हैं:

```cpp
void setup() {
  Serial.begin(9600); // 9600 बॉड दर पर सीरियल संचार की प्रारंभिक स्थापना
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // त्रुटि संदेश को पूर्ववर्ती करके stderr का अनुकरण
    Serial.println("त्रुटि: फंक्शन ने क्रियान्वयन में विफल रहा।");
  } else {
    Serial.println("फंक्शन सफलतापूर्वक क्रियान्वित हुआ।");
  }
  delay(1000); // लूप को पुन: प्रारंभ करने से पहले एक सेकंड के लिए प्रतीक्षा करें
}

int someFunction() {
  // एक डमी फंक्शन जो त्रुटि पर -1 लौटाता है
  return -1;
}
```

Arduino IDE सीरियल मॉनिटर में नमूना आउटपुट इस प्रकार दिखाई दे सकता है:

```
त्रुटि: फंक्शन ने क्रियान्वयन में विफल रहा।
```

विभिन्न भौतिक आउटपुट्स पर लिखने सहित अधिक परिष्कृत दृष्टिकोण की मांग करने वाली परियोजनाओं के लिए, तृतीय-पक्ष पुस्तकालयों का उपयोग या अतिरिक्त हार्डवेयर की आवश्यकता हो सकती है। उदाहरण के लिए, एसडी कार्ड पर त्रुटि संदेशों को लॉग करना `SD` पुस्तकालय की मांग क�
