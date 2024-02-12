---
title:                "मानक त्रुटि के लिए लिखना"
date:                  2024-02-03T19:33:38.232073-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Arduino प्रोग्रामिंग में मानक त्रुटि (stderr) के लिए लेखन का मतलब है त्रुटि संदेशों और निदानों को एक अलग चैनल पर निर्देशित करना, सुनिश्चित करना कि वे मानक आउटपुट (stdout) के साथ मिले नहीं। प्रोग्रामर इसे नॉर्मल प्रोग्राम आउटपुट्स से त्रुटि संदेशों को अलग करने के लिए करते हैं, जिससे डिबगिंग और लॉग विश्लेषण अधिक सरल हो जाते हैं।

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