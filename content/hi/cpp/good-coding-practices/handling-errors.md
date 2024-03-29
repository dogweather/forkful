---
date: 2024-01-26 00:53:17.870180-07:00
description: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\
  \u0932 \u0915\u0930\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948\
  \ \u0915\u093F \u091A\u0940\u095B\u0947\u0902 \u0917\u0932\u0924 \u0939\u094B\u0928\
  \u0947 \u092A\u0930 \u0909\u0928\u0915\u0947 \u0932\u093F\u090F \u092A\u094D\u0932\
  \u093E\u0928\u093F\u0902\u0917 \u0915\u0930\u0928\u093E\u0964 \u092F\u0939 \u0906\
  \u0935\u0936\u094D\u092F\u0915 \u0939\u0948 \u0915\u094D\u092F\u094B\u0902\u0915\
  \u093F \u092F\u0939 \u0915\u094D\u0930\u0948\u0936\u0947\u091C \u0915\u094B \u091F\
  \u093E\u0932\u0928\u0947 \u092E\u0947\u0902 \u092E\u0926\u0926 \u0915\u0930\u0924\
  \u093E \u0939\u0948 \u0914\u0930 \u0906\u092A\u0915\u0947 \u0938\u0949\u092B\u094D\
  \u091F\u0935\u0947\u092F\u0930 \u0915\u094B\u2026"
lastmod: '2024-03-13T22:44:52.857561-06:00'
model: gpt-4-1106-preview
summary: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\
  \u093F \u091A\u0940\u095B\u0947\u0902 \u0917\u0932\u0924 \u0939\u094B\u0928\u0947\
  \ \u092A\u0930 \u0909\u0928\u0915\u0947 \u0932\u093F\u090F \u092A\u094D\u0932\u093E\
  \u0928\u093F\u0902\u0917 \u0915\u0930\u0928\u093E\u0964 \u092F\u0939 \u0906\u0935\
  \u0936\u094D\u092F\u0915 \u0939\u0948 \u0915\u094D\u092F\u094B\u0902\u0915\u093F\
  \ \u092F\u0939 \u0915\u094D\u0930\u0948\u0936\u0947\u091C \u0915\u094B \u091F\u093E\
  \u0932\u0928\u0947 \u092E\u0947\u0902 \u092E\u0926\u0926 \u0915\u0930\u0924\u093E\
  \ \u0939\u0948 \u0914\u0930 \u0906\u092A\u0915\u0947 \u0938\u0949\u092B\u094D\u091F\
  \u0935\u0947\u092F\u0930 \u0915\u094B\u2026"
title: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
एरर्स को हैंडल करने का मतलब है कि चीज़ें गलत होने पर उनके लिए प्लानिंग करना। यह आवश्यक है क्योंकि यह क्रैशेज को टालने में मदद करता है और आपके सॉफ्टवेयर को मजबूत और उपयोगकर्ता-हितैषी बनाता है।

## कैसे:
यहाँ एक बुनियादी try-catch ब्लॉक है जो एक एक्सेप्शन को हैंडल करता है:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("ओह! कुछ गलत हो गया।");
    } catch (const std::exception& e) {
        std::cerr << "एरर: " << e.what() << std::endl;
    }
    return 0;
}
```

नमूना आउटपुट:
```
एरर: ओह! कुछ गलत हो गया।
```

## गहराई से समझे
C++ में एरर हैंडलिंग इसके प्रारम्भिक दिनों से है। सबसे मूल रूप वापसी मानों की जांच था। अगर आप पुराने जमाने को जानते हैं, तो आप 'C with classes' और मैनुअल एरर चेकिंग को याद करते होंगे।

फिर C++ के साथ एक्सेप्शंस आए, जिससे हमें अप्रत्याशित समस्याओं के साथ निपटने का एक सुसंगठित तरीका मिला। एक एक्सेप्शन `throw` के साथ फेंका जाता है और `try/catch` के साथ पकड़ा जाता है।

दो प्रकार की एरर्स अक्सर सामने आती हैं: लॉजिकल एरर्स, जैसे कि गलत गणना, और रनटाइम एरर्स, जैसे कि एक अमान्य मेमोरी एड्रेस का एक्सेस करना। एक्सेप्शंस रनटाइम एरर्स के लिए आदर्श हैं। लॉजिक एरर्स के लिए अक्सर अस्सर्शन्स या एरर कोड्स का उपयोग करना बेहतर होता है।

एक्सेप्शंस बनाम एरर कोड्स पर लगातार बहस चल रही है। एक्सेप्शंस धीमे हो सकते हैं और अधिक जटिल कंट्रोल फ्लोज का कारण बन सकते हैं। एरर कोड्स, जबकि तेज होते हैं, कोड को अस्त-व्यस्त और बनाए रखने के लिए कठिन बना सकते हैं। यह एक ट्रेड-ऑफ होता है, इसलिए आपके उपयोग के मामले को जानना महत्वपूर्ण है।

C++17 ने `std::optional` और `std::variant` पेश किया, जो एक्सेप्शंस के विकल्प हैं। वे उन फंक्शन्स के लिए उपयोगी हैं जो एक मान्य परिणाम वापस कर सकते हैं या नहीं।

एक्सेप्शन सेफ्टी एक और सिरदर्द हो सकती है। यह आपके कोड द्वारा एक्सेप्शंस के बावजूद प्रदान किए गए गारंटी के बारे में है। तीन स्तर होते हैं: बेसिक, स्ट्रॉंग, और नोथ्रो। अधिक गारंटी, उतना अधिक जटिल आपका कोड हो सकता है।

अंतिम विचार — एरर हैंडलिंग उतनी ही कला है जितनी विज्ञान। यह तय करता है कि आपका एप्लिकेशन जंगल में कैसे जीवित रहता है। एक्सेप्शंस का अत्यधिक उपयोग न करें। पठनीय, रख-रखाव में आसान कोड के लिए लक्ष्य बनाएं।

## देखे भी
- [cppreference पर एक्सेप्शन हैंडलिंग](https://en.cppreference.com/w/cpp/language/exceptions)
- [ब्यार्ने स्ट्रौस्ट्रुप की एरर हैंडलिंग पर राय](http://www.stroustrup.com/except.pdf)
- [C++ कोर गाइडलाइन्स पर एक्सेप्शंस](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
