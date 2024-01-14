---
title:    "Java: तारीख को स्ट्रिंग में रूपांतरण करना"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें अपने जावा प्रोग्राम में तिथि को स्ट्रिंग में रूपांतरित करने की आवश्यकता हो सकती है। इसके लिए हमें `SimpleDateFormat` का उपयोग करना पड़ता है।

## कैसे करें

यहां हम `SimpleDateFormat` का उपयोग करके एक तारीख स्ट्रिंग में रूपांतरित करने का एक छोटा सा उदाहरण दिखाएंगे:

``` Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateFormatExample {
    public static void main(String[] args) {
        Date date = new Date();
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        String dateString = formatter.format(date);
        
        System.out.println(dateString); // Output: 25/11/2021
    }
}
```

यहां हमने `SimpleDateFormat` का एक ऑब्जेक्ट बनाया और इसके उपयोग से वर्तमान तारीख को `dd/MM/yyyy` फॉर्मेट में स्ट्रिंग में रूपांतरित कर लिया है। आप चाहें तो अपनी इच्छानुसार इस फॉर्मेट को बदल सकते हैं। इसके अलावा आप अन्य फॉर्मेट भी लागू कर सकते हैं जैसे `yyyy-MM-dd` या `hh:mm:ss`। 

## गहराई में जाएं

साल-महीने-दिन के अलावा आप `SimpleDateFormat` का उपयोग करके अन्य तारीख प्रकारों को भी स्ट्रिंग में रूपांतरित कर सकते हैं। जैसे कि समय (`hh:mm:ss`) या समय और तारीख दोनों का एक साथ (`dd/MM/yyyy hh:mm:ss`)। भी साथ ही आप `SimpleDateFormat` के साथ अपनी खुद की फॉर्मेट भी निर्दिष्ट कर सकते हैं। यहां आप `SimpleDateFormat` की डॉक्यूमेंटेशन पढ़ कर अधिक जानकारी प्राप्त कर सकते हैं। 

## इससे जुड़े अन्य लेख

- [Java में `SimpleDateFormat` का उपयोग कैसे करें?](https://www.geeksforgeeks.org/java-setting-date-format/)
- [कैसे Java में तारीखों का समय निर्धारित करें](https://www.javatpoint