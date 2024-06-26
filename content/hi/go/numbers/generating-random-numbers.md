---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:35.168020-07:00
description: "\u0915\u0948\u0938\u0947: Go \u092E\u0947\u0902, \u092F\u093E\u0926\u0943\
  \u091A\u094D\u091B\u093F\u0915 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u093E \u0909\u0924\u094D\u092A\u093E\u0926\u0928 `math/rand` \u092A\u0948\
  \u0915\u0947\u091C \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u0928\u0915\u0932\u0940-\u092F\u093E\u0926\u0943\u091A\u094D\u091B\u093F\
  \u0915 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\
  \u090F \u092F\u093E `crypto/rand` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0915\u0947 \u0915\u094D\u0930\u093F\u092A\u094D\u091F\u094B\u0917\u094D\u0930\
  \u093E\u092B\u093F\u0915\u2026"
lastmod: '2024-04-05T21:53:53.428718-06:00'
model: gpt-4-0125-preview
summary: "Go \u092E\u0947\u0902, \u092F\u093E\u0926\u0943\u091A\u094D\u091B\u093F\u0915\
  \ \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u093E \u0909\u0924\u094D\
  \u092A\u093E\u0926\u0928 `math/rand` \u092A\u0948\u0915\u0947\u091C \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0928\u0915\u0932\u0940\
  -\u092F\u093E\u0926\u0943\u091A\u094D\u091B\u093F\u0915 \u0938\u0902\u0916\u094D\
  \u092F\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\u090F \u092F\u093E `crypto/rand`\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0915\u094D\
  \u0930\u093F\u092A\u094D\u091F\u094B\u0917\u094D\u0930\u093E\u092B\u093F\u0915 \u0930\
  \u0942\u092A \u0938\u0947 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 \u0928\
  \u0915\u0932\u0940-\u092F\u093E\u0926\u0943\u091A\u094D\u091B\u093F\u0915 \u0938\
  \u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\u090F \u0915\
  \u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0906\u0907\u090F\
  \ \u0926\u094B\u0928\u094B\u0902 \u0915\u094B \u0926\u0947\u0916\u0947\u0902\u0964\
  ."
title: "\u092F\u093E\u0926\u0943\u091A\u094D\u091B\u093F\u0915 \u0938\u0902\u0916\u094D\
  \u092F\u093E\u0913\u0902 \u0915\u093E \u0928\u093F\u0930\u094D\u092E\u093E\u0923"
weight: 12
---

## कैसे:
Go में, यादृच्छिक संख्याओं का उत्पादन `math/rand` पैकेज का उपयोग करके नकली-यादृच्छिक संख्याओं के लिए या `crypto/rand` का उपयोग करके क्रिप्टोग्राफिक रूप से सुरक्षित नकली-यादृच्छिक संख्याओं के लिए किया जाता है। आइए दोनों को देखें।

### नकली-यादृच्छिक संख्याओं के लिए `math/rand` का उपयोग
पहले, `math/rand` पैकेज और जेनरेटर को सीड करने के लिए `time` पैकेज को आयात करें। सीडिंग सुनिश्चित करती है कि आपको हर रन में अलग अलग अनुक्रम मिलें।

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("A random number:", rand.Intn(100)) // 0 और 99 के बीच की एक संख्या उत्पन्न करता है
}
```

नमूना उत्पादन: `A random number: 42`

### क्रिप्टोग्राफिक रूप से सुरक्षित नकली-यादृच्छिक संख्याओं के लिए `crypto/rand` का उपयोग
अधिक सुरक्षा-संवेदनशील अनुप्रयोगों के लिए, `crypto/rand` पैकेज उपयुक्त है क्योंकि यह यादृच्छिक संख्याओं का उत्पादन करता है जिन्हें अनुमान लगाना कठिन होता है, जिससे वे क्रिप्टोग्राफिक परिचालनों के लिए उपयुक्त होते हैं।

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("A secure random number:", n)
}
```

नमूना उत्पादन: `A secure random number: 81`

## गहराई में जानकारी
`math/rand` और `crypto/rand` पैकेज के बीच का मूल भेद Go में उनके एंट्रोपी के स्रोत और उनके इरादे के मामलों से संबंधित है। `math/rand` एक प्रारंभिक बीज पर आधारित नकली-यादृच्छिक संख्याएँ उत्पन्न करता है; इस प्रकार, अनुक्रम निर्धारित होता है और बीज ज्ञात होने पर इसका अनुमान लगाया जा सकता है। यह सिमुलेशन या खेलों जैसे परिदृश्यों के लिए उपयुक्त है जहां उच्च प्रदर्शन और संपूर्ण अप्रत्याशितता की चिंता मुख्य नहीं है।

दूसरी ओर, `crypto/rand` अंतर्निहित संचालन प्रणाली से यादृच्छिकता प्राप्त करता है, जिससे यह क्रिप्टोग्राफिक उपयोगों के लिए उपयुक्त होता है जहां अप्रत्याशितता अत्यंत महत्वपूर्ण है। हालांकि, इसके प्रदर्शन और इसके द्वारा उत्पन्न संख्याओं को संभालने की जटिलता (जैसे कि पूर्णांकों के लिए `*big.Int` प्रकार का संबंध) की लागत आती है।

ऐतिहासिक रूप से, कंप्यूटरों में यादृच्छिक संख्या उत्पादन की धारणा हमेशा सच्ची "यादृच्छिकता" की सीमा पर नृत्य कर रही है, जिसमें शुरुआती प्रणालियों ने यादृच्छिकता की नकल करने वाले निर्धारित एल्गोरिदम पर भारी निर्भरता दिखाई। कंप्यूटरों के विकसित होने के साथ, ये एल्गोरिदम भी विकसित हुए, अपने वातावरण से अधिक सूक्ष्म स्रोतों की एंट्रोपी को शामिल करते हुए।

इन उन्नतियों के बावजूद, कंप्यूटिंग में पूर्ण यादृच्छिकता की खोज स्वाभाविक रूप से विरोधाभासी है, देखते हुए कि कंप्यूटर स्वयं निर्धारित प्रकृति के होते हैं। यही कारण है कि, ज्यादातर अनुप्रयोगों के लिए जहां प्रत्याशितता हानिकारक होगी, `crypto/rand` जैसे स्रोतों से क्रिप्टोग्राफिक रूप से सुरक्षित नकली-यादृच्छिक संख्याएँ बेहतर विकल्प होती हैं, इसके बावजूद कि इसमें अतिरिक्त लागत आती है।

सारांश में, Go का दो विशिष्ट पैकेजों के लिए यादृच्छिक संख्या उत्पादन के दृष्टिकोण प्रदर्शन और सुरक्षा के बीच समझौतों को सुंदरतापूर्वक संबोधित करता है, विकासकों को उनकी विशिष्ट आवश्यकताओं के आधार पर चुनने की अनुमति देता है।
