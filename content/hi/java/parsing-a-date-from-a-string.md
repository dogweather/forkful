---
title:                "स्ट्रिंग से तारीख का अनुक्रमण करना"
html_title:           "Java: स्ट्रिंग से तारीख का अनुक्रमण करना"
simple_title:         "स्ट्रिंग से तारीख का अनुक्रमण करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों?
डेट फाइल की स्ट्रिंग से पार्सिंग क्या है, यह प्रोग्रामर्स इसलिए करते हैं कि वे उस स्ट्रिंग से डेट को प्राप्त कर सकें।

## कैसे करें:
```Java
import java.text.SimpleDateFormat;
import java.util.Date;
public class DateParser {
  public static void main(String[] args) {
    SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
    Date date = sdf.parse("25/12/2020");
    System.out.println(date);
  }
}
```
और भी अधिक तारीख पार्सिंग कोड और उनका परिणाम आप नीचे देख सकते हैं:

```Java
SimpleDateFormat sdf = new SimpleDateFormat("dd-MM-yyyy");
Date date = sdf.parse("25-12-2020");
System.out.println(date); //Fri Dec 25 00:00:00 IST 2020

SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yy");
Date date = sdf.parse("12/25/20");
System.out.println(date); //Fri Dec 25 00:00:00 IST 2020
```

## गहराई में जाएं:
प्रारंभिक रूप से, डेट पार्सिंग एक विशिष्ट तारीख फॉर्मेट के साथ डेट ऑब्जेक्ट को प्राप्त करने का एक तरीका था। हालांकि, आजकल बहुत सारे अलग-अलग तारीख स्ट्रिंग फॉर्मेट हैं, इसलिए हमें उचित तारीख पार्सिंग लोजिक को लिखना हो सकता है। अन्य विकल्पों में, हम एक डेट पार्सिंग लाइब्रेरी भी प्रयोग कर सकते हैं जो हमारे लिए यह सब काम करती है। अंत में, डेट पार्सिंग के लिए एक ऊंचा स्तरीय विवरण के अंतर्गत तीन विभाग हैं - फ़ोर्मेट स्ट्रिंग, डेट पार्सिंग लोजिक, और तारीख परिणाम को फॉर्मेट करने के लिए टेम्पलेट।

## देखें भी:
- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [SimpleDateFormat Class](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Date Parsing in Java using Regular Expressions](https://dzone.com/articles/java-date-parsing-using-regular-expressions)