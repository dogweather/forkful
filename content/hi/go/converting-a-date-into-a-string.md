---
title:    "Go: एक तारीख को स्ट्रिंग में रूपांतरण"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप अपने गो प्रोग्राम में तारीख को स्ट्रिंग में बदलने की जरूरत महसूस कर रहे हैं? शायद आपको अपने उपयोगकर्ताओं को एक स्पष्ट तारीख डिस्प्ले करनी हो या अपने डेटाबेस में स्टोर करने के लिए स्ट्रिंग में तारीख को कन्वर्ट करना चाहिए। इस ब्लॉग पोस्ट में, हम आपको गो प्रोग्रामिंग में तारीख को स्ट्रिंग में कन्वर्ट करने के लिए उपयोगी तरीके दिखाएंगे।

## कैसे करें

कैसे अपने गो प्रोग्राम में तारीख को स्ट्रिंग में कन्वर्ट करें? इसके लिए निम्नलिखित कोड ब्लॉक को अपने प्रोग्राम में समायोजित करें।

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    today := time.Now()
    
    // Convert date into string using standard format
    str := today.Format("01/02/2006")
    fmt.Println("Converted date into string: ", str)
    
    // Convert date into string using custom format
    str = today.Format("2006-01-02")
    fmt.Println("Converted date into string: ", str)
}
```

आपको अपने कंप्यूटर पर निम्नलिखित आउटपुट मिलना चाहिए:

```
Converted date into string: 11/11/2021
Converted date into string: 2021-11-11
```

यहां आप देख सकते हैं कि हमने `Format()` फंक्शन को दो पैरामीटर पास किए हैं - पहले में हमने अपनी डेट के तारीख को फॉर्मेट करने के तरीके को उसी फॉर्मेट में बताया है जिसमें हम उस तारीख को डिस्प्ले करना चाहते हैं। दूसरे पैरामीटर में हमने अपने स्ट्रिंग के फॉर्मेट को बताया है जिसमें हम अपनी तारीख को बदलना चाहते हैं। आप अपने रुचानुसार इन फॉर्मेट को बदल सकते हैं।

## गहराई म