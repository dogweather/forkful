---
title:    "Go: दो तिथियों का तुलना करना"
keywords: ["Go"]
---

{{< edit_this_page >}}

## क्यों?

क्या आप कभी दो तारीखों को अपने प्रोग्राम में तुलना करने के बारे में सोचा है? अगर हाँ, तो आप सही जगह पर हो! आज हम आपको बताएंगे कि गोलांग में दो तारीखों की तुलना कैसे की जाती है और इसके क्या फायदे हो सकते हैं।

## कैसे करें?

दो तारीखों को गोलांग में तुलना करने के लिए हमें `time` पैकेज का उपयोग करना होगा। नीचे दिए गए कोड ब्लॉक में हमने इसका एक सरल उदाहरण दिखाया है:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	today := time.Now()
	
	// 1 day = 24 hours = 24 * 60 minutes = 24 * 60 * 60 seconds
	yesterday := today.Add(-24 * time.Hour) // Subtracting 24 hours from current time
	diff := today.Sub(yesterday)
	
	fmt.Println(diff.Hours(), "hours have passed since yesterday.")
}
```

आपको इस कोड को रन करने पर निम्नलिखित आउटपुट मिलेगा:

```
24 hours have passed since yesterday.
```

इसके अलावा, आप दो तारीखों के बीच difference को `Days()` या `Minutes()` आदि का उपयोग करके भी प्राप्त कर सकते हैं। आप समय को भी अपनी मनचाही फॉर्मेट में दिखा सकते हैं।

## गहराईगामी

दो तारीखों की तुलना करने के बारे में और गहराई जानने के लिए, आप गोलांग के अन्य समय और तारीख से सम्बंधित पैकेज की जानकारी खोज सकते हैं। इसमें इन्हें शामिल किया जा सकता है `time.Format()` और `time.Parse()` जो आपको समय को किसी अन्य फॉर्मेट में परिवर्तित करने देंगे।

## देखें भी

- [गोलांग आधिकारिक time पैकेज दस्तावेज़ीकरण](https://golang.org/pkg/time/)
- [डेटा प्रबंधन के बेहतरीन तरीके गोलांग