---
title:                "Java: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

आपने अपने प्रोग्राम में दो तारीखों की तुलना क्यों करनी है, इसके बारे में जानने के लिए आप सही जगह पर हैं। तारीखों को तुलना करने का एक साधारण कारण है कि हमें दो तारीखों के बीच कितने अंतर या कितने दिन का है पता करना होता है। इसके अलावा, इसके सभी प्रोग्रामिंग टूल ?हिन्‍दी हो या अंग्रेजी? में तारीखों के साथ गणितीय ऑपरेशन अहम हैं।

## कैसे करें

यदि आप दो तारीखों की तुलना करना चाहते हैं, तो आपको सरल और सटीक तरीके से कोडिंग करना होगा। नीचे दिए गए उदाहरण में आप जावा कोड ब्लॉक के भीतर से संबंधित कोड देख सकते हैं।

```Java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

// दो तारीखों का अंतर का हिसाब करें
LocalDate date1 = LocalDate.of(2020, 11, 15);
LocalDate date2 = LocalDate.of(2020, 12, 25);

// तारीखों के बीच दिनों का अंतर
long daysBetween = ChronoUnit.DAYS.between(date1, date2);
System.out.println("तारीख 1 से तारीख 2 तक " + daysBetween + " दिन हैं।");

// तारीखों के बीच मासों का अंतर
long monthsBetween = ChronoUnit.MONTHS.between(date1, date2);
System.out.println("तारीख 1 से तारीख 2 तक " + monthsBetween + " माह हैं।");
```

इस कोड का नतीजा निम्न रूप में होगा:

```Output
तारीख 1 से तारीख 2 तक 40 दिन हैं।
तारीख 1 से तारीख 2 तक 1 माह हैं।
```

## गहराई में जाएं

तारीखों को तुलना करने से पहले, हमें जावा में तारीखों को प्रदर्शित करने और मानकीकृत करने की आवश्यकता होती है। आप ज