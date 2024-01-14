---
title:    "Arduino: भविष्य या भूतकाल में एक दिन की गणना"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## क्यों
क्या आप कभी अपने आर्डुइनो प्रोजेक्ट में भविष्य में या भूतकाल की तारीख की गणना करने की जरूरत महसूस करते हैं? आर्डुइनो में तारीख की गणना करने से आप अपने प्रोजेक्ट को और भी अधिक उपयोगी बना सकते हैं!

## कैसे करें
```Arduino
#include <TimeLib.h>
#include <TimeAlarms.h>

void setup() {
  // काल का प्रारंभ करें
  DateTime now = DateTime(2020, 8, 10, 12, 0, 0); // मौजूदा तारीख और समय (वर्ष, माह, दिन, घंटे, मिनट, सेकंड)
  setTime(now.unixtime());

  // भविष्य में तारीख की गणना करें
  time_t futureDate = now.unixtime() + (60 * 60 * 24 * 7); // 7 दिन बाद की तारीख
  DateTime future = DateTime(futureDate);
  Serial.println("Future Date: " + String(future.year()) + "/" + String(future.month()) + "/" + String(future.day()));

  // भूतकाल की तारीख की गणना करें
  time_t pastDate = now.unixtime() - (60 * 60 * 24 * 7); // 7 दिन पहले की तारीख
  DateTime past = DateTime(pastDate);
  Serial.println("Past Date: " + String(past.year()) + "/" + String(past.month()) + "/" + String(past.day()));
}

void loop() {
  // रीटर्न करें
  Serial.println("Done!");
  while (true) {}
}
```
```
आप अपने आर्डुइनो प्रोजेक्ट में `DateTime` और `unixtime` फंक्शन का उपयोग करके भविष्य और भूतकाल की तारीख की गणना कर सकते हैं। उपरोक्त कोड आपको मौजूदा तारीख से 7 दिन आगे और पहले की तारीख प्रदान करेगा। इसके अलावा, आप अपनी आवश्यकतानुसार अन्य समय इंटरवल का भी उपयोग कर सकते हैं।

## गहराई तक जाएं
आप भविष्य और भूतकाल की तारीख की गणना करके अपने प्रोजेक्ट में समय प्रबंधन को और भी उन्नत बना सकते हैं। आप इस