---
title:    "Go: नियमित अभिव्यक्तियों का उपयोग करना"
keywords: ["Go"]
---

{{< edit_this_page >}}

## क्यों

रेगुलर एक्सप्रेशन का उपयोग करने का कारण है कि यह एक शक्तिशाली तकनीक है जो टेक्स्ट स्ट्रिंगों को खोजने और परिवर्तित करने में मदद कर सकती है। यह कोर Go प्रोग्रामिंग भाषा में भी समर्थित है जो इसे एक पसंदीदा विकल्प बनाता है।

## कैसे करें

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// एक साधारण या भिन्न हिसाब से शब्दों की पहचान करने के लिए नए एक्सप्रेशन बनाएं
	r := regexp.MustCompile("गो")

	// संबंधित कोड को चलाएं और स्ट्रिंग तापस्या को प्रिंट करें
	fmt.Println(r.MatchString("मुझे गो पसंद है"))
	fmt.Println(r.MatchString("मुझे काफी ज्ञान गो की जरूरत है"))
	fmt.Println(r.MatchString("लेकिन मैं गो प्रोग्रामिंग को ठीक से नहीं समझता"))
}
```

#### Output:

> true
> true
> false

इस उदाहरण में, हमने सामान्य एक्सप्रेशन "गो" को नए एक साधारण बनाया है और इसका उपयोग तीन अलग-अलग शब्दों के साथ किया है। जैसा कि आप देख सकते हैं, प्रत्येक समय प्रिंट आउट अपने हिसाब से सही है और यह आसानी से हमारे कोड में एक त्रुटि का पता लगा सकता है। इसके अलावा, हम और भी गो में प्राबद्ध संरक्षक तकनीकों का उपयोग कर सकते हैं जो हमारे कोड को और भी शक्तिशाली बना सकते हैं।

## गहराई में जाएं

रेगुलर एक्सप्रेशन का उपयोग करने का एक अन्य महत्वपूर्ण उदाहरण है डेटा वैलिडेशन और संदर्भ सुविधाएं। अधिक गहराई में ज