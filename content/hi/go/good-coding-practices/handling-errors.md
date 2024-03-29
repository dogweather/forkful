---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:29.101524-07:00
description: "Go \u092E\u0947\u0902 \u0924\u094D\u0930\u0941\u091F\u093F\u092F\u094B\
  \u0902 \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0928\u093E \u0906\u092A\u0915\
  \u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u092E\u0947\u0902\
  \ \u0924\u094D\u0930\u0941\u091F\u093F \u0938\u094D\u0925\u093F\u0924\u093F\u092F\
  \u094B\u0902 \u0915\u094B \u092A\u0939\u091A\u093E\u0928\u0928\u0947 \u0914\u0930\
  \ \u0909\u0928\u0915\u093E \u091C\u0935\u093E\u092C \u0926\u0947\u0928\u0947 \u0915\
  \u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0924\u094D\u0930\
  \u0941\u091F\u093F \u0938\u0902\u092D\u093E\u0932\u0928\u0947 \u0915\u093E \u0915\
  \u093E\u092E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u2026"
lastmod: '2024-03-13T22:44:51.446985-06:00'
model: gpt-4-0125-preview
summary: "Go \u092E\u0947\u0902 \u0924\u094D\u0930\u0941\u091F\u093F\u092F\u094B\u0902\
  \ \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0928\u093E \u0906\u092A\u0915\u0947\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u092E\u0947\u0902 \u0924\
  \u094D\u0930\u0941\u091F\u093F \u0938\u094D\u0925\u093F\u0924\u093F\u092F\u094B\u0902\
  \ \u0915\u094B \u092A\u0939\u091A\u093E\u0928\u0928\u0947 \u0914\u0930 \u0909\u0928\
  \u0915\u093E \u091C\u0935\u093E\u092C \u0926\u0947\u0928\u0947 \u0915\u0940 \u092A\
  \u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0924\u094D\u0930\u0941\u091F\u093F\
  \ \u0938\u0902\u092D\u093E\u0932\u0928\u0947 \u0915\u093E \u0915\u093E\u092E \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902\u2026"
title: "\u0924\u094D\u0930\u0941\u091F\u093F\u092F\u094B\u0902 \u0915\u093E \u0938\
  \u092E\u093E\u0927\u093E\u0928"
---

{{< edit_this_page >}}

## क्या और क्यों?

Go में त्रुटियों को संभालना आपके प्रोग्राम में त्रुटि स्थितियों को पहचानने और उनका जवाब देने की प्रक्रिया है। प्रोग्रामर त्रुटि संभालने का काम करते हैं ताकि उनके अनुप्रयोग अप्रत्याशित स्थितियों से सुचारू रूप से उबर सकें, जिससे अधिक मजबूत और विश्वसनीय सॉफ्टवेयर बनते हैं।

## कैसे:

Go में, त्रुटि संभालना स्पष्ट रूप से `error` प्रकार का उपयोग करके प्रबंधित किया जाता है। विफल हो सकने वाले फंक्शन्स अपने अंतिम रिटर्न मूल्य के रूप में एक त्रुटि वापस करते हैं। यह जांचना कि यह त्रुटि मूल्य `nil` है, आपको यह बताएगा कि कोई त्रुटि हुई है या नहीं।

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("मान 100 या उससे कम होना चाहिए")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("त्रुटि:", err)
    } else {
        fmt.Println("परिणाम:", result)
    }
    
    // एक त्रुटि को सुचारु रूप से संभालना
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("त्रुटि:", anotherErr)
    } else {
        fmt.Println("परिणाम:", anotherResult)
    }
}
```

उपरोक्त कोड के लिए नमूना आउटपुट:
```
त्रुटि: मान 100 या उससे कम होना चाहिए
परिणाम: 100
```

इस उदाहरण में, `Compute` फंक्शन या तो गणित किया हुआ मान या एक त्रुटि लौटाता है। कॉलर त्रुटि को यह जांचकर संभालता है कि `err` `nil` नहीं है।

## गहराई में

Go का त्रुटि संभालने का दृष्टिकोण जानबूझकर सीधा और प्रकार-सुरक्षित है, जिसमें त्रुटियों की स्पष्ट जांच की आवश्यकता होती है। यह अवधारणा Java और Python जैसी भाषाओं में देखे गए अपवाद-आधारित त्रुटि संभालने के साथ विपरीत है, जहां त्रुटियों को कॉल स्टैक के ऊपर प्रसारित किया जाता है जब तक कि उन्हें एक अपवाद हैंडलर द्वारा पकड़ा नहीं जाता। Go टीम का तर्क है कि त्रुटियों का स्पष्ट रूप से संभालना स्पष्ट और अधिक विश्वसनीय कोड का परिणाम देता है, क्योंकि यह प्रोग्रामर को वहां जहां वे होती हैं, तुरंत त्रुटियों को संबोधित करने पर मजबूर करता है।

हालांकि, कुछ आलोचनाओं का कहना है कि यह पैटर्न, विशेषकर कई त्रुटि-प्रवण ऑपरेशनों वाले जटिल फंक्शनों में, वर्बोस कोड की ओर ले जा सकता है। जवाब में, Go के नए संस्करणों ने अधिक परिष्कृत त्रुटि संभालने की सुविधाओं, जैसे कि त्रुटि लपेटना, पेश किया है, जिससे मूल त्रुटि जानकारी को खोए बिना त्रुटि को संदर्भ प्रदान करना आसान हो जाता है। समुदाय ने नए त्रुटि संभालने की मैकेनिज्म, जैसे कि check/handle के प्रस्तावों को भी देखा है, हालांकि ये मेरे आखिरी अपडेट के अनुसार चर्चा के अधीन रहते हैं।

Go की त्रुटि संभालने की दर्शन त्रुटियों को कार्यक्रम के सामान्य प्रवाह के एक हिस्से के रूप में समझने और योजना बनाने पर जोर देती है। यह दृष्टिकोण अधिक लचीले और पूर्वानुमानित सॉफ़्टवेयर के विकास को प्रोत्साहित करता है, हालांकि बूटस्ट्रैप कोड में संभावित वृद्धि के साथ। विशेषकर जटिल मामलों के लिए त्रुटि संभालने को सरल बनाने के लिए वैकल्पिक पैटर्न और लाइब्रेरियां मौजूद हैं, लेकिन Go का बिल्ट-इन `error` प्रकार भाषा में त्रुटि संभालने की नींव बना हुआ है।
