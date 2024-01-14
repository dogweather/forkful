---
title:                "Java: तारीख को प्राप्त करना"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि कंप्यूटर कैसे तारीख और समय को जानता है? यदि हाँ, तो आप शायद ये भी जानना चाहेंगे कि आप ऐसा कैसे कर सकते हैं। इस ब्लॉग पोस्ट के माध्यम से मैं आपको बताऊंगा कि आप अपने जावा प्रोग्राम में दैनिक तारीख कैसे प्राप्त कर सकते हैं।

## कैसे

```Java
import java.time.LocalDate;

public class DateExample{
    public static void main(String[] args){
        // वर्तमान दिनांक को प्राप्त करें
        LocalDate today = LocalDate.now();
        System.out.println("आज का दिनांक: " + today);

        // अन्य संभावित फार्मेट के साथ दिनांक प्राप्त करें
        LocalDate customDate = LocalDate.of(2021, 8, 15);
        System.out.println("मेरा जन्म तिथि: " + customDate);

        // दिनांक में विशेष विवरण जैसे महीना और साल प्राप्त करें
        int month = today.getMonthValue();
        int year = today.getYear();
        System.out.println("वर्तमान महीना: " + month);
        System.out.println("वर्तमान साल: " + year);
    }
}
```

**आउटपुट:**
```
आज का दिनांक: 2021-08-29
मेरा जन्म तिथि: 2021-08-15
वर्तमान महीना: 8
वर्तमान साल: 2021
```

## डीप डाइव

जब हम जावा प्रोग्राम में `LocalDate.now()` फंक्शन का उपयोग करते हैं, तो वह वर्तमान समय क्षेत्र की दिनांक और समय को लौटाता है। यह Java 8 वर्जन में जोड़ी गई एक नई क्लास है जो दिनांक, समय और समय क्षेत्र को हैंडल करती है।

दिनांक कार्यक्षेत्र निर्धारित करना भी आसान है, हम इसे मदद से स्थानीय दिनांक क्षेत्र और `of()` फंक्शन का उपयोग कर सकते हैं। `of()` फंक्शन को वर्गीकृत करने के लिए आप साल,