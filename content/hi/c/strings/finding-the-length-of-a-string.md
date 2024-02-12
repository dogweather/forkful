---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
date:                  2024-02-03T17:57:28.159250-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/finding-the-length-of-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
C में एक स्ट्रिंग की लंबाई निर्धारित करना शून्य समाप्ति के पहले के अक्षरों की संख्या का पता लगाने में शामिल होता है। प्रोग्रामर इस काम को बफर ओवरफ्लो जैसी त्रुटियों में चलने के बिना सही ढंग से स्ट्रिंग्स को संचालित करने के लिए करते हैं, जो सुरक्षा संवेदनशीलताओं या कार्यक्रम की दुर्घटनाओं का कारण बन सकता है।

## कैसे:
C में, मानक पुस्तकालय फ़ंक्शन `strlen()` का सामान्यतः उपयोग एक स्ट्रिंग की लंबाई खोजने के लिए किया जाता है। यहाँ एक त्वरित उदाहरण है:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Length of '%s' is %zu.\n", myString, length);
    
    return 0;
}
```

**नमूना आउटपुट:**
```
'Hello, World!' की लंबाई 13 है।
```

इस उदाहरण में, `strlen()` इनपुट के रूप में एक स्ट्रिंग (`myString`) लेता है और शून्य समाप्ति को छोड़कर इसकी लंबाई लौटाता है। लंबाई वेरिएबल के लिए `size_t` का उपयोग करने की सिफारिश की जाती है क्योंकि यह एक अनसाइन इंटेजर प्रकार है, जिससे वह सिस्टम पर संभव सबसे बड़ी वस्तु के आकार को दर्शा सकता है।

## गहन विवरण:
`strlen()` फ़ंक्शन भाषा के आरंभ से C मानक पुस्तकालय का हिस्सा रहा है। अंतत:, यह एक काउंटर को बढ़ाते हुए काम करता है क्योंकि यह स्ट्रिंग के माध्यम से चलता है जब तक यह शून्य समाप्ति में नहीं आ जाता। हालांकि, इस सादगी के साथ प्रदर्शन संबंधी विचार भी आते हैं: क्योंकि `strlen()` रनटाइम में अक्षरों की गिनती करता है, इसलिए इसे उदाहरण के लिए, एक ही स्ट्रिंग पर एक लूप में बार-बार बुलाना अप्रभावी है।

सुरक्षा के लिहाज से, `strlen()` और सी के अन्य स्ट्रिंग-संभालने वाले फंक्शन स्वाभाविक रूप से बफर ओवररन के लिए जाँच नहीं करते हैं, जिससे कि संवेदनशीलताओं से बचने के लिए सावधानीपूर्वक प्रोग्रामिंग अनिवार्य होती है। अन्य भाषाओं में आधुनिक विकल्प, जैसे कि लंबाई शामिल करनेवाले स्ट्रिंग टाइप्स या सुरक्षित बफर संभाल डिफ़ॉल्ट रूप से उपयोग, कुछ इन जोखिमों और अक्षमताओं को समाप्त करते हैं।

इसकी सीमाओं के बावजूद, C में `strlen()` और मैनुअल स्ट्रिंग संभालने की समझ विशेषकर तब महत्वपूर्ण हो जाती है जब कम-स्तरीय कोड के साथ काम करना होता है या जब प्रदर्शन और स्मृति नियंत्रण परम महत्वपूर्ण होते हैं। यह अन्य भाषाओं में उच्च-स्तरीय स्ट्रिंग अमूर्तताओं की कार्यप्रणाली के बारे में मूल्यवान अंतर्दृष्टि भी प्रदान करता है।