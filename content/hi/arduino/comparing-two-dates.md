---
title:    "Arduino: दो तारीखों की तुलना"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# क्यों

यदि आप अर्दुइनो के प्रोग्रामिंग दुनिया में थोड़ा शानदार हो तो आप जानते होंगे कि तारीखों को पैरामीटर के रूप में अदा करना बहुत महत्वपूर्ण है। यह आदेश चोरी से रोकने से लेकर चट्टानों के बीच से घुसे हुए शीतल से प्लास्टिक मोटर को चलाने जैसे अन्य महत्वपूर्ण कामों के लिए उपयोगी हो सकता है। इस ब्लॉग पोस्ट में हम आपको बताएंगे कि आप कैसे अर्दुइनो की मदद से दो तारीखों को तुलना कर सकते हैं।

# कैसे

'''अर्दुइनो कोड मैंने। यह अर्दुइनो कोड है जो भिन्न तारीखों को तुलना करता है। यह उदाहरण तारीख 14 मार्च 2021 और 20 मार्च 2021 को तुलना करता है।

```Arduino
#include <Arduino.h>
#include <TimeLib.h>
#include <Time.h>

void setup() {
  Serial.begin(9600);
  setTime(20, 00, 00, 01, 01, 2021);
}

void loop() {
  //जांचें कि तारीख 14 मार्च 2021 के तारीख से बड़ा है या नहीं
  if(year() > 2021 || month() > 3 || day() > 14)
    Serial.println("यह तारीख 14 मार्च 2021 के तारीख से बड़ा है");
  
  //जांचें कि तारीख 20 मार्च 2021 के तारीख के बराबर है या नहीं
  if(year() == 2021 && month() == 3 && day() == 20)
    Serial.println("यह तारीख 20 मार्च 2021 के तारीख के बराबर है");
  
}
```
'''


जैसा कि आप देख सकते हैं, हमने सबसे पहले आरंभिक सेटअप में समय को सेट किया है। उनके बाद, हमने दो अलग-अलग तारीखों को तुलना करने के लिए दो अलग-अलग शर्तें जोड़ी हैं