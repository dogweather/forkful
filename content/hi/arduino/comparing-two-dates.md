---
title:                "Arduino: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों
**डेटों को तुलना करने में इंजाम**
डेटों को तुलना करने से आप दो प्रमुख तारीखों के बीच किसी भी समय अंतर को निर्धारित कर सकते हैं। यह बहुत उपयोगी हो सकता है जब आपको दो घटनाओं के बीच अंतर निर्धारित करना हो, जैसे कि जन्मदिनों, आयु, और अवस्थाएं।

## कैसे करें
कोड उदाहरण और सैंपल आउटपुट के साथ "```अर्डुइनो ...```" कोड ब्लॉक्स के भीतर दिखाएँ। आप निम्न एक आर्डुइनो कोड द्वारा दो तारीखों को तुलना कैसे कर सकते हैं:

```arduino
#include <TimeLib.h> //टाइम बाइब्रेरी को शामिल करें

// दो मौजूदा तारीख वैरिएबल्स घोषित करें
tmElements_t existingDate1, existingDate2;

// दूसरी तारीख को नई तारीख की तरह सेट करें
existingDate2.Year = 2020;
existingDate2.Month = 12;
existingDate2.Day = 31;

// पहली तारीख की अवस्था सेट करें
setTime(12, 0, 0, 1, 1, 2019); // जनवरी 1, 2019 को दोपहर 12 बजे
existingDate1 = breakTime(now()); // अब की तारीख को टाइम के रूप में ब्रेक करें

// दो तारीखों के बीच अंतर निर्धारित करें
time_t difference = makeTime(existingDate2) - makeTime(existingDate1); // सेकंड्स के रूप में अंतर वापस दें
int days = 0; // दिन की संख्या शुरू करें
if (difference >= SECS_PER_DAY) { // यदि अंतर एक दिन से अधिक है,
  days = difference / SECS_PER_DAY; // दिनों को संख्यांक में बदलें
}
// अंतर को सिकंडों से हटाएं और सिकंडों की संख्या को प्रिंट करें
Serial.print("दिनों का अंतर: "); 
Serial.println(days);
```

आउटपुट:
```
दिनों क