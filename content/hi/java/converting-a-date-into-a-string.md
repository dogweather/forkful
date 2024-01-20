---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डेटा को स्ट्रिंग में बदलना का मतलब होता है तारीख को एक ऐसे स्वरूप में बदलना जिसे हम पठन या प्रदर्शन के लिए उपयोग कर सकते हैं। यह तब जरूरी होता है जब हमें तारीख को उपयोगकर्ता को प्रदर्शित करने की आवश्यकता होती है, या जब हमें डेटा को एक विशेष स्वरूप (जैसे ययांमद्दद - YYYY/MM/DD) में सहेजना होता है। 

## कैसे:

नीचे दिए गए कोड उदाहरण देखें:

```Java
import java.text.SimpleDateFormat;  
import java.util.Date;  

public class Example {  
  public static void main(String[] args) {  
    SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");  
    Date date = new Date();  
    System.out.println(formatter.format(date));  
  }  
}  
```
सैंपल आउटपुट:

```Java
06/01/2022 14:35:15
```

## गहराई में:

1. हिस्टोरिकल कांटेक्स्ट: जावा प्रोग्रामिंग भाषा में डेट और समय को स्ट्रिंग में बदलने की क्षमता की मांग हमेशा से रही है।

2. विकल्प: `SimpleDateFormat `विकल्प ही नहीं, बल्कि `java.time.format.DateTimeFormatter` श्रेणी का उपयोग भी कर सकते हैं। यह जावा 8 के साथ आता है और ISO 8601 मानक का समर्थन करता है।

3. आवेग विवरण: `SimpleDateFormat` वर्ग का `format` विधि `Date` वर्ग के ऑब्जेक्ट को एक स्ट्रिंग में बदलती है, जिसका प्रारूप आपने निर्धारित किया है।

## देखें भी: 

1. Java Documentation: [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html) 

2. Java Documentation: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html) 

3. [HowToDoInJava: Java Convert Date to String](https://howtodoinjava.com/java/date-time/java-convert-date-to-string/)