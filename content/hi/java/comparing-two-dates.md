---
title:                "दो तिथियों की तुलना करना"
html_title:           "Java: दो तिथियों की तुलना करना"
simple_title:         "दो तिथियों की तुलना करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

दो तारीखों को तुलना करने में जुटने का *क्यों* कोई कारण हो सकता है। यह माहत्वपूर्ण हो सकता है जब हमें दो घटनाओं के बीच का अंतर जानने की आवश्यकता होती है, जैसे दो लोगों के जन्मदिन की तारीखों को तुलना करना जो हमें उनके उम्र को पता करने में मदद कर सकता है।

## कैसे

```Java
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
// दो तारीखों को एक समान मान के साथ सामान्य प्रारूप में परिवर्तित करें
Date date1 = sdf.parse("12/01/2022");
Date date2 = sdf.parse("22/12/2021");

// isBefore() का उपयोग करके दोनों दिन की सामान्यता की तुलना करें।
if (date1.isBefore(date2)) {
  System.out.println("पहली तारीख दूसरी तारीख से पहले है।");
} else {
  System.out.println("दूसरी तारीख पहली तारीख से पहले है।");
}
```

आउटपुट:

```
पहली तारीख दूसरी तारीख से पहले है।
```

## गहराई में जाएं

तारीखों को तुलना करने के लिए विभिन्न मेथड का उपयोग कर सकते हैं, जैसे `LocalDate` या `Calendar` के साथ लोगों के समय क्षेत्रों का ध्यान रखते हुए। आप भी अपनी आवश्यकताओं के अनुसार तारीख का अंतर निकाल सकते हैं जैसे साल, महीना, दिन, घंटे आदि। एक उदाहरण है जहां आप हमारी दो तारीखों को महीनों में तुलना करते हुए दो तारीखों के बीच के महीनों का अंतर निकाल सकते हैं।

```Java
LocalDate date1 = LocalDate.of(2021, Month.OCTOBER, 15);
LocalDate date2 = LocalDate.of(2023, Month.MARCH, 31);
// दो तारीखों के बीच के महीनों का अंतर
long monthsApart = ChronoUnit.MONTH