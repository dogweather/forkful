---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV (Comma-separated values) एक साधारण फाइल प्रारूप है जो तालिका-आकार के डेटा को स्टोर करता है। प्रोग्रामर्स CSV का इस्तेमाल डेटा एक्सचेंज, डेटा विश्लेषण, और डेटाबेस इंटरऑपरेबिलिटी के लिए करते हैं, क्योंकि यह सीधे स्वरूप में होता है और अधिकांश प्रोग्रामिंग लैंग्वेजेस और डेटाबेस सिस्टम्स के साथ सहज संगत होता है।

## How to: (कैसे करें:)
Java में CSV फाइल पढ़ने के लिए, हम Apache Commons CSV लाइब्रेरी का इस्तेमाल कर सकते हैं। सबसे पहले इस लाइब्रेरी को अपने प्रोजेक्ट में जोड़ें। Maven का इस्तेमाल करके आप निम्न डिपेंडेंसी को `pom.xml` में जोड़ सकते हैं:

```java
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.8</version>
</dependency>
```

अब Java कोड की मदद से CSV पढ़ने का एक उदाहरण देखें:

```java
import java.io.FileReader;
import java.io.Reader;
import org.apache.commons.csv.*;

public class CSVReaderExample {
    public static void main(String[] args) {
        try (Reader reader = new FileReader("data.csv");
             CSVParser csvParser = new CSVParser(reader, CSVFormat.DEFAULT.withFirstRecordAsHeader())) {
            
            for (CSVRecord csvRecord : csvParser) {
                String name = csvRecord.get("Name");
                String email = csvRecord.get("Email");
                System.out.println("Name: " + name + ", Email: " + email);
            }
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

यदि `data.csv` इस तरह दिखती है:
```
Name,Email
John Doe,johndoe@email.com
Jane Doe,janedoe@email.com
```

तब आउटपुट होगा:
```
Name: John Doe, Email: johndoe@email.com
Name: Jane Doe, Email: janedoe@email.com
```

## Deep Dive (गहन अध्ययन):
CSV का इस्तेमाल 1970 के दशक से हो रहा है। मुख्य विकल्प JSON, XML, और YAML हैं, जो अधिक जटिल डेटा संरचनाओं को संभाल सकते हैं, पर CSV अपनी सादगी के लिए पसंदीदा है। Java में CSV को संभालते समय हमें परफॉर्मेंस और मेमोरी उपयोग का विशेष ध्यान रखना पड़ता है, खासकर बड़ी फाइलों के साथ। 

## See Also (और भी देखें):
- Apache Commons CSV official documentation: [https://commons.apache.org/proper/commons-csv/](https://commons.apache.org/proper/commons-csv/)
- OpenCSV library, another popular CSV library for Java: [http://opencsv.sourceforge.net/](http://opencsv.sourceforge.net/)
- Java documentation for handling I/O: [https://docs.oracle.com/javase/tutorial/essential/io/](https://docs.oracle.com/javase/tutorial/essential/io/).
