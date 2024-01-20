---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

विभाजन ("पार्सिंग") किसी विशेष दिनांक ("डेट") को एक वर्णमाला ("स्ट्रिंग") से प्राप्त करना इत्यादि का एक क्रियाकलाप है। प्रोग्रामर इसे डेटा का व्यवस्थित रूप से हैंडल करने और मशीन को समझाने के लिए करते हैं।

## कैसे करें:

आइए JDK 8 और इसके ऊपर के संस्करणों में दिनांक को पार्स करने का उदाहरण देखते हैं। 

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        String dateString = "2022-02-05";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate parsedDate = LocalDate.parse(dateString, formatter);
        System.out.println(parsedDate);
    }
}
```

ऊपरी कोड स्निपेट का परिणाम होगा:

```
2022-02-05
```

## गहरी जानकारी:

जावा भाषा में दिनांक पार्स करने का इतिहास छोटा है, लेकिन इसके कई रूप हैं। उदाहरण के लिए, पहले `SimpleDateFormat` का उपयोग किया जाता था, जो अब आमतौर पर `DateTimeFormatter` द्वारा बदल दिया गया है। 

`SimpleDateFormat` के अपेक्षा, `DateTimeFormatter` में कई बड़े लाभ हैं, जैसे कि thread safety और बेहतर एपीआई। 

किंतु, अगर आप एक पुराने कोडबेस के साथ काम कर रहे हैं, तो आपको शायद `SimpleDateFormat` का सामना करना पड़े।

## और देखें:

- Oracle Java Documentation: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Baeldung: [Guide to LocalDate](https://www.baeldung.com/java-8-date-time-intro)
- StackOverflow: [Parsing date from String using Java 8 DateTimeFormatter](https://stackoverflow.com/questions/22463062/how-to-parse-format-dates-with-localdatetime-java-8)