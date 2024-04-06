---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:24.433015-07:00
description: "\u0915\u0948\u0938\u0947: \u0915\u094B\u091F\u0932\u093F\u0928 \u0915\
  \u0940 \u0905\u092A\u0928\u0940 \u0915\u094B\u0908 \u0926\u093F\u0928\u093E\u0902\
  \u0915 \u0914\u0930 \u0938\u092E\u092F API \u0928\u0939\u0940\u0902 \u0939\u0948\
  , \u0932\u0947\u0915\u093F\u0928 \u092F\u0939 \u0907\u0938 \u0915\u093E\u0930\u094D\
  \u092F\u0915\u094D\u0937\u092E\u0924\u093E \u0915\u0947 \u0932\u093F\u090F \u091C\
  \u093E\u0935\u093E \u0938\u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092A\u0930 \u0928\u093F\u0930\
  \u094D\u092D\u0930 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\
  \u0901 \u0906\u092A \u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0926\u093F\u0928\
  \u093E\u0902\u0915 \u0915\u0948\u0938\u0947\u2026"
lastmod: '2024-03-13T22:44:52.277005-06:00'
model: gpt-4-0125-preview
summary: "\u0915\u094B\u091F\u0932\u093F\u0928 \u0915\u0940 \u0905\u092A\u0928\u0940\
  \ \u0915\u094B\u0908 \u0926\u093F\u0928\u093E\u0902\u0915 \u0914\u0930 \u0938\u092E\
  \u092F API \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\u0915\u093F\u0928\
  \ \u092F\u0939 \u0907\u0938 \u0915\u093E\u0930\u094D\u092F\u0915\u094D\u0937\u092E\
  \u0924\u093E \u0915\u0947 \u0932\u093F\u090F \u091C\u093E\u0935\u093E \u0938\u094D\
  \u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940 \u092A\u0930 \u0928\u093F\u0930\u094D\u092D\u0930 \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u0906\u092A \u0935\u0930\u094D\
  \u0924\u092E\u093E\u0928 \u0926\u093F\u0928\u093E\u0902\u0915 \u0915\u0948\u0938\
  \u0947 \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930 \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902."
title: "\u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0924\u093E\u0930\u0940\u0916\
  \ \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E"
weight: 29
---

## कैसे:


### मानक कोटलिन का उपयोग करके
कोटलिन की अपनी कोई दिनांक और समय API नहीं है, लेकिन यह इस कार्यक्षमता के लिए जावा स्टैंडर्ड लाइब्रेरी पर निर्भर करता है। यहाँ आप वर्तमान दिनांक कैसे प्राप्त कर सकते हैं:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("आज की तारीख: $today")
}
```

**नमूना आउटपुट:**
```
आज की तारीख: 2023-04-05
```

### java.util.Date का उपयोग करके
जिन ऑपरेशनों को दिनांक और समय दोनों की आवश्यकता होती है, आप `java.util.Date` पसंद कर सकते हैं।

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("वर्तमान दिनांक और समय: $currentDate")
}
```

**नमूना आउटपुट:**
```
वर्तमान दिनांक और समय: Wed Apr 05 15:20:45 GMT 2023
```

### Joda-Time पुस्तकालय का उपयोग करके
जावा 8 ने एक नई दिनांक और समय API पेश की थी, उससे पहले Joda-Time जावा और कोटलिन में दिनांक-समय ऑपरेशनों के लिए डी-फैक्टो मानक था। हालाँकि अब यह कई प्रोजेक्ट्स के लिए आवश्यक नहीं है, कुछ इसे अभी भी पुराने कारणों या व्यक्तिगत पसंद के लिए उपयोग कर सकते हैं।

अपनी प्रोजेक्ट की build.gradle फ़ाइल में Joda-Time पुस्तकालय जोड़ें:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("आज की तारीख: $today")
}
```

**नमूना आउटपुट:**
```
आज की तारीख: 2023-04-05
```

### Android के लिए ThreeTenABP का उपयोग करके
Android विकास के लिए, Android API Level 26 से पहले के संस्करणों के लिए जावा टाइम API की बैकपोर्ट वाया थ्रीटेन एंड्रॉइड बैकपोर्ट प्रोजेक्ट का उपयोग करना सुझाया जाता है।

अपने एप की build.gradle फ़ाइल में निर्भरता जोड़ें:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

इसे अपने एप्लिकेशन क्लास में प्रारंभ करें:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

फिर, आप इसे इस तरह उपयोग कर सकते हैं:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("आज की तारीख: $today")
}
```

**नमूना आउटपुट:**
```
आज की तारीख: 2023-04-05
```
