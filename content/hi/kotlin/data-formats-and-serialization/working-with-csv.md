---
title:                "CSV के साथ काम करना"
aliases:
- hi/kotlin/working-with-csv.md
date:                  2024-02-03T19:21:30.930803-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV (Comma-Separated Values) के साथ काम करना सादा पाठ में तालिका डेटा संग्रहीत करने के एक सामान्य प्रारूप, CSV फ़ाइलों से डेटा पढ़ने और उस पर लिखने का काम शामिल है। प्रोग्रामर्स विभिन्न अनुप्रयोगों, डेटाबेसों के बीच डेटा का आसानी से आदान-प्रदान करने या डेटा प्रोसेसिंग और विश्लेषण कार्य सुविधाजनक बनाने के लिए CSV फ़ाइलों का मेनिपुलेशन करते हैं।

## कैसे करें:

कोटलिन, एक स्टैटिकली टाइप्ड प्रोग्रामिंग भाषा है जो JVM पर चलती है, लेकिन इसमें CSV फाइलों को संभालने के लिए एक निर्मित पुस्तकालय शामिल नहीं होता है। हालांकि, आप बेसिक ऑपरेशनों के लिए जावा `BufferedReader` और `FileWriter` क्लासों का उपयोग कर सकते हैं, या `kotlinx.serialization` और `opencsv` जैसी लोकप्रिय तीसरे पक्ष की लाइब्रेरीज का लाभ उठाकर अधिक उन्नत फंक्शनलिटी के लिए कर सकते हैं।

### BufferedReader का उपयोग करके CSV फ़ाइल पढ़ना:

```kotlin
import java.io.BufferedReader
import java.io.FileReader

fun main() {
    val path = "data.csv"
    val br = BufferedReader(FileReader(path))
    br.useLines { lines ->
        lines.forEach { line ->
            val cols = line.split(',')
            println(cols)
        }
    }
}
```

_नमूना आउटपुट:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### FileWriter का उपयोग करके CSV फ़ाइल में लिखना:

```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Name", "Age", "City"),
        listOf("John Doe", "30", "New York"),
        listOf("Jane Smith", "25", "London")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

इससे `output.csv` नामक फ़ाइल में दी गई डेटा के साथ बनाया जाएगा या उसे अधिलेखित किया जाएगा।

### kotlinx.serialization का उपयोग करके CSV Serialization:

सबसे पहले, अपनी `build.gradle.kts` में निर्भरता जोड़ें:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_नोट: आपके पास सही संस्करण और रेपोजिटरी कॉन्फ़िगरेशन होना जरूरी है।_

फिर, अपना डेटा क्लास परिभाषित करें और Serialization के लिए `Csv` प्रारूप का उपयोग करें:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Person(val name: String, val age: Int, val city: String)

fun main() {
    val csvFormat = Csv { delimiter = ',' }
    val data = listOf(
        Person("John Doe", 30, "New York"),
        Person("Jane Smith", 25, "London")
    )

    val csvData = csvFormat.encodeToString(data)
    println(csvData)
}
```

_नमूना आउटपुट:_

```
John Doe,30,New York
Jane Smith,25,London
```

### OpenCSV का उपयोग करके उन्नत ऑपरेशंस:

अपनी प्रोजेक्ट की निर्भरताओं में OpenCSV जोड़ें:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

OpenCSV के साथ पढ़ने और लिखने:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // CSV पढ़ना
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // CSV लिखना
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val entries = listOf(
            arrayOf("Name", "Age", "City"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "London")
        )
        csvWriter.writeAll(entries)
    }
}
```

ये कोड स्निपेट्स कोटलिन की CSV फ़ाइलों के साथ काम करते समय की पेशकश की गई लचीलापन को दर्शाते हैं, जिससे आप अपनी परियोजना की आवश्यकताओं के लिए सर्वोत्तम विधि का चयन कर सकते हैं।
