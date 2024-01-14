---
title:    "Java: दो तिथियों का तुलना करना"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

"## क्यों"
एक सरल उत्तर हो सकता है यह कि तारीखों की तुलना करने का सबसे आसान और उपयोगी तरीका है तारीखों के बीच का क्या अंतर है। यह हमें अपने कोड में सही तारीखों का उपयोग करने में मदद करता है और समय समझ नापने के लिए अनुकूल तारीखों की व्यवस्था करता है।

"## कैसे करें"
```Java
import java.time.LocalDate;
import java.time.Period;

// दो तारीखों को तुलना करें
LocalDate date1 = LocalDate.of(2019, 12, 25);
LocalDate date2 = LocalDate.now();

//दोनों तारीखों के बीच अंतर को प्राप्त करें
Period period = Period.between(date1, date2);

System.out.println("कार्यान्वयन में " + period.getMonths() + " महीने और "
        + period.getDays() + " दिन हैं।");
```

जैसे ही ऊपर दिए गए कोड को चलाया जाता है, हमें फर्कों की जानकारी मिलती है और यह संदेश स्क्रीन पर दिखाता है:
```
कार्यान्वयन में 10 महीने और 8 दिन हैं।
```

"## गहराई में जाएँ"
तारीखों की तुलना करने के लिए हम लोकलडेट क्लास के बारे में बात कर रहे हैं जो जावा 8 में नया गठन किया गया है। इसका उपयोग टाइमस्टैम्प कोडिंग के दौरान किया जाता है। हम किसी भी तारीख की जन्मतिथि, शादी की सालगिरह या कभी भी आवश्यक समय समझाने के लिए इस्तेमाल कर सकते हैं।

"## देखें भी"
[Java होमपेज] (https://www.java.com/hi/)
[लोकलपेंस दस्तावेज़ीकरण] (https://www.oracle.com/technetwork/java/javase/documentation/index.html)