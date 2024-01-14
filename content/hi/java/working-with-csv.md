---
title:                "Java: कंप्यूटर प्रोग्रामिंग में काम करना: csv के साथ"
simple_title:         "कंप्यूटर प्रोग्रामिंग में काम करना: csv के साथ"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-csv.md"
---

{{< edit_this_page >}}

## शब्दावली

- CSV - Comma Separated Values (अक्षर विभाजित मान)

## क्यों

CSV (Comma Separated Values) एक प्रसिद्ध डेटा प्रारूप है जो डेटा को अक्षर विभाजित मानों के तौर पर संग्रहीत करता है। एक सामान्य कारण है कि यह विभिन्न सॉफ्टवेयर और प्रोग्रामों के बीच डेटा को संगत बनाने के लिए प्रयोग किया जाता है। CSV अक्षर विभाजित मानों का एक सरल संरचना होने के कारण, यह डेटा को संस्करण करने और उसे अन्य प्रोग्रामों में आसानी से स्थानांतरित करने के लिए बहुत उपयोगी होता है।

## कैसे करें

इस ब्लॉग पोस्ट में, हम जावा में CSV (Comma Separated Values) फ़ाइलें संसाधित करने के लिए कुछ उदाहरण प्रस्तुत करेंगे। यहां हम कुछ सरल कोड स्निपेट द्वारा इसका उपयोग करेंगे।

उदाहरण १: CSV फ़ाइल से डेटा पढ़ना

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class ReadCSV {

    public static void main(String[] args) {
        BufferedReader reader = null;
        List<String> dataList = new ArrayList<>();

        try {
            // CSV फ़ाइल को पढ़ें
            reader = new BufferedReader(new FileReader("data.csv"));
            String line;

            // प्रत्येक लाइन को अक्षर विभाजित मान में विभाजित करें
            while ((line = reader.readLine()) != null) {
                String[] data = line.split(",");
                for (String value : data) {
                    // डेटा सूची में डेटा जोड़ें
                    dataList.add(value);
                }
            }

            // फ़ाइल को समाप्त करें
            reader.close();

        } catch (IOException e) {
            e.printStackTrace();
        }

        // डेटा सूची में से डेटा प्रिंट करें
        for (String data : dataList) {
            System.out.println(data);
        }
    }
}
```

आउटपुट:

```text
1
John Doe
30
USA
```

उदाहरण २: CSV फ़ाइल में डेटा लिखना

```Java
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class WriteCSV {

    public static void main(String[] args) {