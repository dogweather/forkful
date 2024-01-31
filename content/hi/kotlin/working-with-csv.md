---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
simple_title:         "CSV के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV, यानी Comma-separated values, एक ऐसी फाइल है जो डेटा को कॉमा द्वारा अलग करके रखती है। प्रोग्रामर्स इसे तब इस्तेमाल करते हैं जब उन्हें डेटा का आदान-प्रदान, संग्रहित करना, या विश्लेषण करना होता है क्योंकि यह ह्यूमन और मशीन दोनों के लिए समझने में आसान होता है।

## How to: (कैसे करें:)
```Kotlin
// Kotlin में CSV फाइल पढ़ना
fun readCSV(filename: String) {
    val lines = File(filename).readLines()
    lines.forEach { line ->
        val columns = line.split(",")
        println(columns)
    }
}

// CSV फाइल में डेटा लिखना
fun writeCSV(filename: String, data: List<List<String>>) {
    File(filename).bufferedWriter().use { out ->
        data.forEach { row ->
            out.write(row.joinToString(","))
            out.newLine()
        }
    }
}

// सैम्पल उपयोग
fun main() {
    val dataToWrite = listOf(
        listOf("आइडी", "नाम", "उम्र"),
        listOf("1", "राम", "25"),
        listOf("2", "सीता", "23")
    )
    val filename = "example.csv"

    writeCSV(filename, dataToWrite)
    readCSV(filename)
}
```
इससे सैम्पल आउटपुट होगा:
```
[आइडी, नाम, उम्र]
[1, राम, 25]
[2, सीता, 23]
```

## Deep Dive (गहराई से जानकारी)
CSV की शुरूआत 1970s में कंप्यूटर साइंस के विकास के दौर में हुई थी। अब भी यह JSON, XML जैसे मॉडर्न फॉर्मेट्स का विकल्प है क्योंकि यह सरल और विभिन्न प्रोग्राम्स में सुगमतापूर्वक उपयोग हो सकता है। Kotlin में CSV डेटा को कार्यान्वित करते समय आपको स्ट्रगल होने पर third-party लाइब्रेरीज़ जैसे कि `Apache Commons CSV` या `kotlin-csv` का इस्तेमाल कर सकते हैं।

## See Also (और भी देखें)
- Kotlin documentation: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- kotlin-csv library: [https://github.com/doyaaaaaken/kotlin-csv](https://github.com/doyaaaaaken/kotlin-csv)
- Apache Commons CSV: [https://commons.apache.org/proper/commons-csv/](https://commons.apache.org/proper/commons-csv/)
