---
title:                "csv के साथ काम करना"
html_title:           "Kotlin: csv के साथ काम करना"
simple_title:         "csv के साथ काम करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV के साथ काम करना क्या है, और प्रोग्रामर्स इसे क्यों करते हैं। CSV स्प्रेडशीट डेटा को कॉमा separated values के रूप में संग्रहीत करता है, जिसे प्रोग्रामर्स डेटा को संसाधित और व्यवस्थित करने के लिए उपयोग कर सकते हैं।

## कैसे करें:

```Kotlin
import java.io.FileReader
import com.opencsv.CSVReader

val csvReader = CSVReader(FileReader("data.csv"))
val data = csvReader.readAll()
val header = data[0]
val rows = data.drop(1)

for (row in rows) {
    val name = row[0]
    val email = row[1]
    val age = row[2].toInt()
    println("नाम: $name, ईमेल: $email, आयु: $age")
}
```

इस मिसाल में, हम एक CSV फाइल से डेटा पढ़ते हैं और उसे संस्कारित सूची में बदलते हैं। फाइल से पहले पंक्ति अक्षरमाला हमारे डेटा कॉलम को संदर्भित करने के लिए उपयोग की जाती है। हम उस पंक्ति को छोड़कर सभी अन्य पंक्तियों को शामिल करते हैं जो हामारे डेटा को प्रतिनिधित करती हैं।

## डीप डाइव:

CSV फाइलें १९७० के दशक से प्रयुक्त हो रही हैं और ये लोकप्रिय रूप से सामान्य फाइल स्वरूप हैं। यदि आपको CSV के साथ काम करना नहीं होता है, तो आपको मुख्य डेटा संग्रहीत स्वरूप TXT, JSON या XML का उपयोग कर सकते हैं।

और यदि आप एक CSV पार्सर उपलब्ध नहीं है, तो आप स्वयं कोड लिख सकते हैं। एक सामान्य त्रुटि है कि हमारे डेटा में फार्मेटिंग गलत हो सकती है, जो मूल डेटा को संसाधित करने में दिक्कत पैदा कर सकती है। संबंधित लेखों के लिंक नीचे दिए गए हैं।

## देखें भी:

- [Comparison of CSV, JSON, and XML](https://medium.com/gocodee/csv-vs-json-vs-xml-443b3a9e6f27)
- [Handling CSV Formatting Errors](https://stackoverflow.com/questions/19206418/how-do-i-handle-csv-files-when-some-fields-are-quoted-and-some-aren-t-in-kotlin)