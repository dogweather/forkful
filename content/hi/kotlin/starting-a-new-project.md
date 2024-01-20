---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

नया प्रोजेक्ट शुरू करना मतलब एक नई सॉफ्टवेयर उत्पादन की शुरुआत करना। प्रोग्रामर्स इसे करते हैं क्योंकि אओंके पास एक अद्वितीय समस्या होती है जिसका समाधान उन्हें बनाना होता है।

## कैसे करें:

Kotlin में नया प्रोजेक्ट शुरू करने के लिए, आमतौर पर Gradle का उपयोग किया जाता है। यहाँ एक नमूना है:

```Kotlin
plugins {
    kotlin("jvm") version "1.5.31"
}

group = "com.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("stdlib"))
}
```

इस कोड का उद्देश्य Gradle को हमारे प्रोजेक्ट के dependencies सेट करना है और हमें एक JVM के लिए कंपाइल करने की क्षमता देना है।

## गहरा गोता

ऐतिहासिक संदर्भ: Kotlin, JetBrains द्वारा विकसित, जावा के विकास पर आधारित एक खुले स्रोत भाषा है। इसका मुख्य उद्देश्य जावा की कमियों को दूर करना और विकासकारिका को सुधारना है।

विकल्प: अन्य भाषाओं, जैसे कि Java और Scala, भी इसे बनाने के लिए इस्तेमाल किए जा सकते हैं।

अंतर्निर्माण विवरण: बमुश्किल, प्रोजेक्ट का डेटा संगठित करने और सही Kotlin लाइब्रेरिज़, functions, और APIs का चयन करने में सावधानी बरतने की ज़रूरत होती है।

## देखें भी 

1. [Kotlin आधिकारिक वेबसाइट](https://kotlinlang.org/)
2. [Gradle आधिकारिक वेबसाइट](https://gradle.org/)