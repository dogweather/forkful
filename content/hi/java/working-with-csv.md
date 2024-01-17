---
title:                "csv के साथ काम करना"
html_title:           "Java: csv के साथ काम करना"
simple_title:         "csv के साथ काम करना"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-csv.md"
---

{{< edit_this_page >}}

क्या और क्यों?

क्या आपने कभी CSV के साथ काम किया है? CSV स्प्रेडशीट्स के साथ दस्तावेज़ों में आसानी से डेटा संग्रह करने के लिए इस्तेमाल किया जाता है। अधिकांश प्रोग्रामर इसका उपयोग डेटा स्टोरेज और मैनिपुलेशन के लिए करते हैं।

कैसे करें:

```
Java कोड ब्लॉक में कोडिंग उदाहरण और सैंपल आउटपुट के साथ।
```

```
// CSV को लोड करें
Reader reader = new FileReader("myFile.csv");
CSVReader csvReader = new CSVReader(reader);

// पंक्तियों को एक्सेस करें
List<String[]> rows = csvReader.readAll();

// पंक्ति की छवि
for (String[] row : rows) {
    System.out.println(Arrays.toString(row));
}

// नई इनसर्ट करें
String[] newRow = {"1", "John", "Doe"};
rows.add(newRow);

// अपडेट करें
rows.get(0)[1] = "Jane";

// नया CSV फ़ाइल में सहेजें
Writer writer = new FileWriter("newFile.csv");
CSVWriter csvWriter = new CSVWriter(writer);
csvWriter.writeAll(rows);
writer.close();
```

डीप डाइव:

CSV का इतिहास बहुत पुराना है और यह प्रत्येक प्रोग्रामिंग भाषा में उपलब्ध है। अन्य विकल्पों में शामिल हैं JSON, XML और SQL databases। CSV फ़ाइलें अपने सरल स्ट्रक्चर के कारण डेटा को इंटरनली असाधारण तरीके से प्रभावित कर सकती हैं। CSV कोड को समझने के लिए, आपको Java आर्ग्युमेंट डोपिंगी, वायरलोबिलिटी और जी एस पर विस्तार से जानने की आवश्यकता होती है।

सी अल्सो:

जीएसवी संबंधित स्त्रोतों के लिंक।
- [Oracle's CSV Implementation](https://docs.oracle.com/javase/8/docs/api/javax/swing/text/Csv.html)
- [CSV Library for Java](https://www.baeldung.com/java-csv-file-array)
- [Reading and Writing CSV Files in Java](https://www.geeksforgeeks.org/reading-and-writing-csv-files-in-java/)