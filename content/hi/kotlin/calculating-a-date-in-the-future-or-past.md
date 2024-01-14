---
title:                "Kotlin: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# क्यों

किसी को भविष्य में या भूतकाल में तारीख का हिसाब करने में क्यों रूचि हो सकती है, इसे समझने के लिए थोड़ी सी बात करें। 

## कैसे करें 

यदि आपको कोटलिन में एक तारीख की गणना करने की जरूरत है, तो निम्न उदाहरण सहित निर्देशों को फॉलो करें। 

```Kotlin 
import java.time.LocalDate 

// आज की दिनांक से 50 दिन बाद की तारीख को पाएं 
val futureDate = LocalDate.now().plusDays(50) 

// "yyyy-MM-dd" के फॉर्मेट में तारीख प्रिंट करें 
println(futureDate.toString("yyyy-MM-dd")) 

// आज की दिनांक से 2 वर्ष पहले की तारीख को पाएं 
val pastDate = LocalDate.now().minusYears(2) 

// "dd/MM/yyyy" के फॉर्मेट में तारीख प्रिंट करें 
println(pastDate.toString("dd/MM/yyyy")) 
``` 

आउटपुट: 

```
2021-09-01 
28/08/2019 
``` 

## गहराई में जाएं 

अब आप कोटलिन में तारीख की गणना कर सकते हैं, लेकिन इसके पीछे की जानकारी समझना भी महत्वपूर्ण है। कोटलिन में `java.time` पैकेज का उपयोग करके हम तारीख के साथ विभिन्न गणनाओं को कर सकते हैं जैसे कि तारीखों को सम्मिलित करना, अंतर निकालना, साल/महीना/दिन की संख्या कोईबार मापना आदि। 

# देखें भी 

- [कोटलिन में तारीख का प्रसन्नता](https://kotlinlang.org/docs/datetime-frustrations.html) 
- [कोटलिन डेटा और टाइम API दस्तावेज़ीकरण](https://docs.oracle.com/javase/tutorial/datetime/iso/datetime.html)