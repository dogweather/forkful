---
title:                "C: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें दो तारीखों को तुलना करने की जरूरत होती है। इसके पीछे के कारण अलग-अलग हो सकते हैं, जैसे कि कोई इम्पोर्टेंट इवेंट, समारोह, या शिफ्ट आदि। जब हम किसी प्रोग्रामिंग समस्या का सामना करते हैं, तो इस तुलना की जानकारी होना बहुत जरूरी होता है। इस ब्लॉग पोस्ट में, हम आपको दो तारीखों को तुलना करने के तरीके के बारे में बताएंगे।

## कैसे करें

तारीखों को तुलना करने के लिए, हमें दो तारीखों को उस तारीख क्लास का उपयोग करके तुलना करना होगा। यहां हम आपको कुछ उदाहरण दिखाएंगे:

```C
#include <stdio.h>
#include <time.h>

int main() {
  // दो तारीखों को तुलना करें
  struct tm date1 = {0}; // पहली तारीख
  struct tm date2 = {0}; // दूसरी तारीख
  double difference; // तारीखों के बीच का अंतर

  // पहली तारीख सेट करें
  date1.tm_year = 2021 - 1900; // साल
  date1.tm_mon = 6 - 1; // महीना (0 से शुरू होता है)
  date1.tm_mday = 15; // दिन
  date1.tm_hour = 12; // घंटे
  date1.tm_min = 0; // मिनट
  date1.tm_sec = 0; // सेकंड

  // दूसरी तारीख सेट करें
  date2.tm_year = 2021 - 1900; // साल
  date2.tm_mon = 6 - 1; // महीना (0 से शुरू होता है)
  date2.tm_mday = 30; // दिन
  date2.tm_hour = 8; // घंटे
  date2.tm_min = 30; // मिनट
  date2.tm_sec = 0; // सेकंड

  // दोनों तारीखों के बीच का अंतर निकालें
  difference = difftime(mktime(&date2), mktime(&date1));

  // अंतर को सेकंड में प्रिंट करें
  printf("तारीखों के बीच अंतर: %f सेकंड", difference);

  return