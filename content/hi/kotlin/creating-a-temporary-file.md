---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# अस्थायी फ़ाइल बनाना Kotlin में

## क्या और क्यों?

अस्थायी फ़ाइलें वे फ़ाइलें होती हैं जिन्हें हम केवल कुछ समय के लिए या एक कार्यक्रम के चलने तक बनाते हैं। प्रोग्रामर इसे डाटा संग्रहीत करने, कामयाबी या विफलता की जांच पड़ताल करने, गणना के बीच अस्थायी परिवर्तन संग्रहीत करने, आदि के लिए उपयोग करते हैं।

## कैसे करें:

Kotlin में अस्थायी फ़ाइल बनाने का कोड इस प्रकार है:

``` kotlin
import java.io.File

fun main() {
    val tempFile = File.createTempFile("test", ".tmp")
    println("File path: ${tempFile.absolutePath}")
}
```

यह कोड एक `test.tmp` नामक नई अस्थायी फ़ाइल का निर्माण करेगा और उसका पथ छापेगा। उत्तर समान हो सकता है `/tmp/test5036891893972423032.tmp` (पथ सिस्टम पर निर्भर करेगा).

## गहरी जानकारी:

1. ऐतिहासिक संदर्भ: अस्थायी फ़ाइलें कम्प्यूटर के आगमन से ही जुड़ी हुई हैं। ये इसलिए बनाई गईं थीं, ताकि अस्थायी डाटा को संग्रहीत किया जा सके।

2. वैकल्पिक विधियां: Kotlin में, आप `Files.createTempFile()` भी उपयोग कर सकते हैं।

3. कार्यान्वयन विवरण: `File.createTempFile()` मेथड केवल फ़ाइल बनाता है, इसे किसी विशेष डायरेक्टरी में नहीं डालता है। आपको पथ को स्वयं सेट करना होगा यदि आप इसे विशिष्ट स्थान पर रखना चाहते हैं।

## देखने के लिए भी:


2. [StackOverflow: कैसे अस्थायी फ़ाइलें Kotlin में बनाएं](https://stackoverflow.com/questions/5694385/creating-temporary-files-in-android)