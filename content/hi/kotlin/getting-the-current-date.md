---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-02-03T19:11:24.433015-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामिंग में, वर्तमान दिनांक प्राप्त करना एक मौलिक कार्य है जो डेवलपर्स को उनके एप्लिकेशन्स में वर्तमान दिनांक को एक्सेस, प्रदर्शित, या मैनिपुलेट करने की क्षमता प्रदान करता है। यह क्षमता घटनाओं के लॉगिंग और समय-चिह्नित करने से लेकर दिनांकों पर आधारित गणनाओं तक के लिए अत्यंत महत्वपूर्ण है।

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
