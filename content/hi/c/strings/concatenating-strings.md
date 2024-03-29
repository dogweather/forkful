---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:27.224316-07:00
description: "C \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0938\u0902\u092F\u094B\u091C\u0928 \u0926\u094B \u092F\u093E \u0905\u0927\u093F\
  \u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B\
  \ \u0905\u0902\u0924-\u0938\u0947-\u0905\u0902\u0924 \u0924\u0915 \u091C\u094B\u0921\
  \u093C\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\
  \u093E \u0939\u0948 \u0924\u093E\u0915\u093F \u090F\u0915 \u0928\u0908 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u092C\u0928\u093E\u0908 \u091C\u093E \u0938\
  \u0915\u0947\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \ \u0907\u0938 \u0915\u093E\u0930\u094D\u092F \u0915\u094B \u0930\u0928\u091F\u093E\
  \u0907\u092E \u092A\u0930\u2026"
lastmod: '2024-03-13T22:44:53.126958-06:00'
model: gpt-4-0125-preview
summary: "C \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\
  \u0902\u092F\u094B\u091C\u0928 \u0926\u094B \u092F\u093E \u0905\u0927\u093F\u0915\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u0905\
  \u0902\u0924-\u0938\u0947-\u0905\u0902\u0924 \u0924\u0915 \u091C\u094B\u0921\u093C\
  \u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E\
  \ \u0939\u0948 \u0924\u093E\u0915\u093F \u090F\u0915 \u0928\u0908 \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u092C\u0928\u093E\u0908 \u091C\u093E \u0938\u0915\
  \u0947\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\
  \u0938 \u0915\u093E\u0930\u094D\u092F \u0915\u094B \u0930\u0928\u091F\u093E\u0907\
  \u092E \u092A\u0930\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B\
  \ \u091C\u094B\u0921\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

C में स्ट्रिंग संयोजन दो या अधिक स्ट्रिंग्स को अंत-से-अंत तक जोड़ने की प्रक्रिया है ताकि एक नई स्ट्रिंग बनाई जा सके। प्रोग्रामर इस कार्य को रनटाइम पर गतिशील रूप से स्ट्रिंग्स का निर्माण करने के लिए करते हैं, जो महत्वपूर्ण संदेश, फाइल पथ, या किसी भी डेटा को विभिन्न स्ट्रिंग स्रोतों से इकट्ठा करके बनाने के लिए अनिवार्य होता है।

## कैसे करें:

C में, स्ट्रिंग्स वर्णों की सरणियाँ होती हैं जो एक शून्य वर्ण (`\0`) के साथ समाप्त होती हैं। उच्च-स्तरीय भाषाओं के विपरीत, C में बिल्ट-इन स्ट्रिंग संयोजन फंक्शन प्रदान नहीं किया जाता है। इसके बजाय, आप `<string.h>` पुस्तकालय से `strcat()` या `strncat()` फंक्शन का उपयोग करते हैं।

यहाँ `strcat()` का उपयोग करते हुए एक साधारण उदाहरण है:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";

    strcat(destination, source);

    printf("%s\n", destination);  // आउटपुट: Hello, World!
    return 0;
}
```

`strcat()` फंक्शन दो तर्क लेता है: गंतव्य स्ट्रिंग (जिसके पास संयुक्त परिणाम को संभालने के लिए पर्याप्त स्थान होना चाहिए) और स्रोत स्ट्रिंग। यह फिर स्रोत स्ट्रिंग को गंतव्य स्ट्रिंग के साथ जोड़ देता है।

जोड़े गए वर्णों की संख्या पर अधिक नियंत्रण के लिए, `strncat()` का उपयोग करना सुरक्षित होता है:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";
    int num = 3; // जोड़ने के लिए वर्णों की संख्या

    strncat(destination, source, num);

    printf("%s\n", destination);  // आउटपुट: Hello, Wor
    return 0;
}
```

यह स्रोत स्ट्रिंग के पहले `num` वर्णों तक संयोजन को सीमित करता है, जो बफर ओवरफ्लो को रोकने में मदद करता है।

## गहराई में

`strcat()` और `strncat()` फंक्शन सी मानक पुस्तकालय का हिस्सा रहे हैं जो भाषा के निम्न-स्तरीय स्वभाव को दर्शाता है जिसमें स्ट्रिंग्स और मेमरी के मैन्युअल प्रबंधन की आवश्यकता होती है। बहुत सी आधुनिक प्रोग्रामिंग भाषाओं के विपरीत जो स्ट्रिंग्स को पहली-क्लास ऑब्जेक्ट्स के रूप में मानती हैं जिसमें बिल्ट-इन संयोजन ऑपरेटर (`+` या `.concat()`) शामिल होते हैं, C का दृष्टिकोण पॉइंटर्स, मेमरी आवंटन, और जैसे बफर ओवरफ्लो जैसी संभावित समस्याओं की गहरी समझ की आवश्यकता होती है।

`strcat()` और `strncat()` का व्यापक उपयोग किया जाता है, लेकिन यदि इनका सावधानीपूर्वक उपयोग नहीं किया जाए तो इनके कारण सुरक्षा कमजोरियों का निर्माण हो सकता है। बफर ओवरफ्लो, जहां डेटा आवंटित मेमरी से अधिक हो जाता है, दुर्घटनाओं का कारण बन सकता है या मनमाने कोड निष्पादन के लिए शोषण किया जा सकता है। परिणामस्वरूप, प्रोग्रामर अधिक सुरक्षित विकल्पों की ओर बढ़ रहे हैं, जैसे कि `snprintf()`, जो गंतव्य स्ट्रिंग के आकार के आधार पर लिखे गए वर्णों की संख्या को सीमित करके अधिक प्रत्याशित व्यवहार प्रदान करता है:

```c
char destination[50] = "Hello, ";
char source[] = "World!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

यह विधि अधिक शब्दजाल होती है लेकिन काफी सुरक्षित है, जो C प्रोग्रामिंग प्रथाओं में संक्षिप्तता की तुलना में सुरक्षा और मजबूती को प्राथमिकता देने की दिशा में बदलाव को उजागर करती है।

इन चुनौतियों के बावजूद, C में स्ट्रिंग संयोजन भाषा में प्रभावी प्रोग्रामिंग के लिए एक आधारभूत कौशल है। इसकी बारीकियों और संबद्ध जोखिमों की समझ C प्रोग्रामिंग में महारत हासिल करने की कुंजी है।
