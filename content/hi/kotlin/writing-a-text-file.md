---
title:                "Kotlin: एक पाठ फ़ाइल लिखना"
simple_title:         "एक पाठ फ़ाइल लिखना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों
कोटलिन में टेक्स्ट फाइल लिखने के लिए हम डेटा को परस्पर एकत्र कर सकते हैं अथवा संग्रहीत कर सकते है।

## कैसे करें
कोटलिन में टेक्स्ट फाइल लिखने के लिए हम `FileWriter` क्लास का उपयोग कर सकते हैं। यहां हम एक प्रखंड को `text.txt` नाम की नई फाइल में लिखने का तरीका दिखाएंगे।

```
Kotlin
val fileName = "text.txt"
val text = "यह एक कोटलिन पाठ है।"
val file = File(fileName)
val writer = FileWriter(file)
writer.append(text)
writer.close()
println("टेक्स्ट फाइल सफलतापूर्वक लिखी गई है।")
```

`text.txt` फाइल का निर्माण होने के बाद, आप निम्नलिखित कोड का उपयोग करके इसे पढ़ सकते हैं:

```
Kotlin
val fileName = "text.txt"
val file = File(fileName)
val reader = FileReader(file)
println(reader.readText())
reader.close()
```

यह आपको निम्नलिखित प्रदर्शित करेगा:
```
यह एक कोटलिन पाठ है।
```

## गहराई में जाएं
टेक्स्ट फाइल लिखना कोई मुश्किल काम नहीं है। कोटलिन में हम `File` और `FileWriter` क्लास का उपयोग कर सकते हैं जो एक नई टेक्स्ट फाइल बनाने और उसमें टेक्स्ट लिखने की सुविधा प्रदान करते हैं। आप भी अपनी टेक्स्ट फाइल में संग्रहीत डेटा को लिखने के लिए `FileReader` क्लास का उपयोग कर सकते हैं। इसके अलावा, आप `BufferedWriter` और `BufferedReader` क्लास का भी उपयोग कर सकते हैं जो आपको फाइल में डेटा को लेखन और पढ़ने के लिए योग्यता प्रदान करते हैं।

## देखें भी
[कोटलिन प्रोग्रामिंग भाषा का परिचय](https://blog.jetbrains.com/kotlin/2020/08/getting-started-with-kotlin/)

[कोटलिन में फाइल