---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:37:20.040481-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख को स्ट्रिंग से पार्स करने का मतलब है स्ट्रिंग फॉर्मेट में दी गई तारीख को Java की Date ऑब्जेक्ट में बदलना। यह जरूरी है क्योंकि यूज़र से मिली इनपुट या डेटाबेस से आंकड़े स्ट्रिंग फॉर्मेट में हो सकते हैं, जिन्हें हमें तारीखों के रूप में प्रोसेस करना होता है।

## How to: (कैसे करें:)
```Java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParsingExample {
    public static void main(String[] args) {
        String dateString = "25-03-2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
       
        try {
            Date date = formatter.parse(dateString);
            System.out.println("Date is: " + date);
        } catch (ParseException e) {
            System.out.println("Invalid format");
        }
    }
}
```
**Sample Output:**
```
Date is: Sat Mar 25 00:00:00 IST 2023
```

## Deep Dive (गहराई से जानकारी):
तारीख पार्सिंग का उपयोग Java में बहुत पहले से होता आ रहा है। `SimpleDateFormat` क्लास इस कार्य के लिए लंबे समय से इस्तेमाल हो रही है और अब Java 8 और उसके बाद के वर्ज़न में `java.time` पैकेज ने कई आसान और लचीले तरीके प्रस्तुत किए हैं। `DateTimeFormatter` और `LocalDate` जैसी क्लासेस के साथ, डेवेलपर्स को ज़्यादा पावरफुल और थ्रेड-सेफ ऑप्शंस मिलते हैं। पार्सिंग में गलतियों से बचने के लिए `try-catch` ब्लॉक का प्रयोग करना चाहिए।

## See Also (और जानकारी के लिए):
- [Date and Time Classes in Java 8](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [SimpleDateFormat Documentation](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Date to LocalDate Conversion](https://www.baeldung.com/java-date-to-localdate-and-localdatetime)