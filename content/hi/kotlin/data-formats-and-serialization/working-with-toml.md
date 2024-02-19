---
aliases:
- /hi/kotlin/working-with-toml/
date: 2024-01-26 04:24:35.134946-07:00
description: "TOML \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 Tom's Obvious,\
  \ Minimal Language. \u092F\u0939 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\
  \u0930\u0947\u0936\u0928 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u0947\
  \ \u0932\u093F\u090F \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u093F\
  \u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948 \u0915\u094D\u092F\u094B\u0902\
  \u0915\u093F \u0907\u0938\u0947 \u092E\u0928\u0941\u0937\u094D\u092F\u094B\u0902\
  \ \u0926\u094D\u0935\u093E\u0930\u093E \u092A\u0922\u093C\u0928\u093E \u0914\u0930\
  \ \u0932\u093F\u0916\u0928\u093E \u0906\u0938\u093E\u0928 \u0939\u094B\u0924\u093E\
  \u2026"
lastmod: 2024-02-18 23:09:03.311583
model: gpt-4-0125-preview
summary: "TOML \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 Tom's Obvious, Minimal\
  \ Language. \u092F\u0939 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\u0947\
  \u0936\u0928 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u0947 \u0932\u093F\
  \u090F \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u093F\u092F\u093E\
  \ \u091C\u093E\u0924\u093E \u0939\u0948 \u0915\u094D\u092F\u094B\u0902\u0915\u093F\
  \ \u0907\u0938\u0947 \u092E\u0928\u0941\u0937\u094D\u092F\u094B\u0902 \u0926\u094D\
  \u0935\u093E\u0930\u093E \u092A\u0922\u093C\u0928\u093E \u0914\u0930 \u0932\u093F\
  \u0916\u0928\u093E \u0906\u0938\u093E\u0928 \u0939\u094B\u0924\u093E\u2026"
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML का मतलब है Tom's Obvious, Minimal Language. यह कॉन्फ़िगरेशन फ़ाइलों के लिए इस्तेमाल किया जाता है क्योंकि इसे मनुष्यों द्वारा पढ़ना और लिखना आसान होता है, साथ ही मशीनों द्वारा पार्स करना भी आसान होता है. डेवलपर्स TOML का उपयोग XML के गड़बड़ी और JSON की चालाकी से बचने के लिए करते हैं जब वे कॉन्फ़िग्स को लागू करते हैं।

## कैसे:
Kotlin में TOML को संभालने के लिए, आप `ktoml` जैसी लाइब्रेरी का उपयोग कर सकते हैं। सबसे पहले, आइए अपने `build.gradle.kts` में डिपेंडेंसी जोड़ें:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

अब, चलिए कुछ TOML को पार्स करते हैं:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val tomlContent = TomlFileReader.readAndParseFile("config.toml")
    
    val databaseConfig = tomlContent.getTable("database")
    val host = databaseConfig.getString("host")
    val port = databaseConfig.getLong("port")

    println("डेटाबेस होस्ट: $host")
    println("डेटाबेस पोर्ट: $port")
}
```

मान लीजिए `config.toml` इस प्रकार दिखता है:

```toml
[database]
host = "localhost"
port = 5432
```

नमूना आउटपुट होगा:

```
डेटाबेस होस्ट: localhost
डेटाबेस पोर्ट: 5432
```

## गहराई से विचार
TOML, जिसे GitHub के सह-संस्थापक Tom Preston-Werner ने 2013 में बनाया था, YAML की तुलना में अधिक सरल और JSON की तुलना में अधिक टाइप-सुरक्षित होने का लक्ष्य रखता था। यह Rust के `Cargo` और Go की मॉड्यूल प्रणाली के साथ खासतौर से लोकप्रिय हो गया है। विकल्प? YAML में अधिक सुविधाएँ हैं, JSON कई कोडिंग भाषाओं में सीधे ऑब्जेक्ट्स में अनुवादित होता है, और हमेशा अच्छा पुराना XML रहेगा। लागू करने के लिए, ktoml, Apache 2.0 लाइसेंस के तहत, एक शुद्ध Kotlin लाइब्रेरी है और इसमें Java लाइब्स को ड्रैग नहीं किया जाता, जो TOML को पढ़ने के अलावा लिखने के लिए भी DSLs प्रदान करती है। 

## देखें भी
- टॉमल GitHub: https://github.com/toml-lang/toml
- केटॉमल GitHub: https://github.com/akuleshov7/ktoml
- TOML बनाम YAML बनाम JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
