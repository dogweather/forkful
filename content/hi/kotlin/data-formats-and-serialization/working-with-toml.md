---
date: 2024-01-26 04:24:35.134946-07:00
description: "\u0915\u0948\u0938\u0947: Kotlin \u092E\u0947\u0902 TOML \u0915\u094B\
  \ \u0938\u0902\u092D\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u0906\
  \u092A `ktoml` \u091C\u0948\u0938\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902\u0964 \u0938\u092C\u0938\u0947 \u092A\u0939\u0932\
  \u0947, \u0906\u0907\u090F \u0905\u092A\u0928\u0947 `build.gradle.kts` \u092E\u0947\
  \u0902 \u0921\u093F\u092A\u0947\u0902\u0921\u0947\u0902\u0938\u0940 \u091C\u094B\
  \u0921\u093C\u0947\u0902."
lastmod: '2024-03-13T22:44:52.298981-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u092E\u0947\u0902 TOML \u0915\u094B \u0938\u0902\u092D\u093E\u0932\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A `ktoml` \u091C\u0948\u0938\
  \u0940 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964 \u0938\u092C\u0938\u0947 \u092A\u0939\u0932\u0947, \u0906\u0907\u090F \u0905\
  \u092A\u0928\u0947 `build.gradle.kts` \u092E\u0947\u0902 \u0921\u093F\u092A\u0947\
  \u0902\u0921\u0947\u0902\u0938\u0940 \u091C\u094B\u0921\u093C\u0947\u0902."
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 39
---

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
