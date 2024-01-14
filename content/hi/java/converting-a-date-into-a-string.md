---
title:                "Java: एक तारीख को एक स्ट्रिंग में बदलना"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## यह क्यों?

कभी-कभी हमें अपने जावा प्रोग्राम में तारीखों के साथ काम करना पड़ता है। शायद हम इसे डेटा बेस के साथ दस्तावेज़ करना चाहते हैं या दिनांकों को संगठित ढंग से प्रदर्शित करना चाहते हैं। ऐसे मामलों में, हमें अपनी तारीख को एक स्ट्रिंग में रूपांतरित करने की आवश्यकता हो सकती है। इस आर्टिकल में, हम इस प्रक्रिया को कैसे करेंगे, इसे समझेंगे। 


## कैसे करें?

हम तारीख को स्ट्रिंग में रूपांतरित करने के लिए `SimpleDateFormat` क्लास का उपयोग कर सकते हैं। `SimpleDateFormat` काफी सारी मुख्य तारीख प्रारूपों को समर्थित करता है जैसे `dd/MM/yyyy` या `MM/dd/yyyy`। इसके अलावा आप `SimpleDateFormat` ऑब्जेक्ट को अपने इच्छानुसार ज़्यादा स्पष्ट प्रारूप भी सेट कर सकते हैं। यहां एक उदाहरण है।

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToString {
    public static void main(String[] args) {
        Date today = new Date(); // वर्तमान समय और दिनांक प्राप्त करें
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy"); // जोड़ी सिर्फ़ उचित प्रारूप
        String stringDate = formatter.format(today); // तारीख को स्ट्रिंग में रूपांतरित करें
        System.out.println("आज की तारीख: " + stringDate);
    }
}
```

आउटपुट:
`आज की तारीख: 23/04/2021`


## गहराई में जाएं

तारीख और समय का एक स्ट्रिंग रूपांतरण करने से पहले, हमें एक `Date` ऑब्जेक्ट बनाना होगा जो वर्तमान दिनांक और समय को अभिव्यक्त करता है। इसके बाद, हम `SimpleDateFormat` क्लास के `format()` मेथड का उपयोग कर स