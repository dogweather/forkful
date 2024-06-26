---
date: 2024-01-26 03:43:44.291668-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u092F\u0939\u093E\
  \u0902 \u0906\u092A\u0915\u0947 TypeScript \u092E\u0947\u0902 \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0938\u0947 \u0909\u0928 \u0915\u0937\
  \u094D\u091F\u092A\u094D\u0930\u0926 \u0909\u0926\u094D\u0927\u0930\u0923 \u091A\
  \u093F\u0939\u094D\u0928\u094B\u0902 \u0915\u094B \u0905\u0932\u0917 \u0915\u0930\
  \u0928\u0947 \u0915\u093E \u0938\u0930\u0932 \u092E\u093E\u0930\u094D\u0917\u0926\
  \u0930\u094D\u0936\u093F\u0915\u093E \u0939\u0948\u0964."
lastmod: '2024-03-13T22:44:51.868421-06:00'
model: gpt-4-0125-preview
summary: "\u092F\u0939\u093E\u0902 \u0906\u092A\u0915\u0947 TypeScript \u092E\u0947\
  \u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0938\u0947\
  \ \u0909\u0928 \u0915\u0937\u094D\u091F\u092A\u094D\u0930\u0926 \u0909\u0926\u094D\
  \u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928\u094B\u0902 \u0915\u094B \u0905\
  \u0932\u0917 \u0915\u0930\u0928\u0947 \u0915\u093E \u0938\u0930\u0932 \u092E\u093E\
  \u0930\u094D\u0917\u0926\u0930\u094D\u0936\u093F\u0915\u093E \u0939\u0948\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0909\u0926\
  \u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\u091F\u093E\u0928\
  \u093E"
weight: 9
---

## कैसे करें:
यहां आपके TypeScript में स्ट्रिंग्स से उन कष्टप्रद उद्धरण चिह्नों को अलग करने का सरल मार्गदर्शिका है।

```typescript
// विकल्प A: रेगेक्स का उपयोग करते हुए एकल या दोहरे उद्धरणों को बदलें
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Quoted string"`)); // Quoted string
console.log(removeQuotes(`'Another one'`)); // Another one

// विकल्प B: ऐसे स्ट्रिंग्स से निपटना जो अलग-अलग उद्धरणों के साथ शुरू और समाप्त होते हैं
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Mismatched'`)); // "Mismatched'

// विकल्प C: विभिन्न प्रकार के उद्धरणों को ट्रिम करना
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## गहराई में जाओ
बहुत पहले, जब TypeScript भी एक बात नहीं थी, जावास्क्रिप्ट कोडर्स पहले से ही उद्धरण की चालबाजियों से निपट रहे थे, और TypeScript के लिए कहानी लगभग वैसी ही है। समय के साथ बदलते ही, हम स्ट्रिंग्स को कैसे काटते हैं, इसमें भी बदलाव होता है। आजकल, रेगेक्स की शक्ति के साथ, हम क्लंकी स्ट्रिंग स्लाइसिंग या अन्य थकाऊ तरीकों का उपयोग करने को एक तरफ धकेल देते हैं।

जबकि ऊपर दिए गए उदाहरण आपकी अधिकांश जरूरतों को कवर करना चाहिए, ध्यान रहे, उद्धरण पेचीदा हो सकते हैं। नेस्टेड, मिसमैच्ड, और एस्केप्ड उद्धरण वे चतुराई लुढ़काने वाले हैं जो आपको गिराने की प्रतीक्षा कर रहे हैं। इनके लिए, आपको अधिक सोफिस्टिकेटेड पैटर्न या यहां तक कि पार्सर्स की आवश्यकता हो सकती है जो हर जटिल मामले को संभाल सकते हो। 

विकल्प? कुछ लोग `trim` और `trimStart` / `trimEnd` जैसे मेथड्स के साथ lodash जैसी लाइब्रेरीज का उपयोग करना पसंद करते हैं, जिन्हें आप उन वर्णों को सेट करके उद्धरणों को काटने के लिए अनुकूलित कर सकते हैं।

और TypeScript के उत्साही लोगों के लिए, चलिए प्रकारों के बारे में न भूलें। जबकि हम यहां मुख्य रूप से स्ट्रिंग्स के साथ काम कर रहे हैं, जब आप उपयोगकर्ता इनपुट या पार्सिंग के साथ काम कर रहे होते हैं, तो कुछ प्रकार के गार्ड्स या यहां तक कि जेनेरिक्स को शामिल करने से आपके कोड को सुरक्षित रखने में मदद मिल सकती है जैसे आपके उद्धरण ट्रिम किए जाते हैं।

## देखें भी
और जानकारी के लिए इन वर्चुअल हॉटस्पॉट्स को देखें:

- MDN वेब डॉक्स पर रेगेक्स (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScript आधिकारिक दस्तावेज़ीकरण (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – स्ट्रिंग हेल्पर्स (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: उन ट्रेंचों को पार करें जहाँ अनगिनत devs ने उद्धरण आपदाओं से लड़ाई लड़ी है (https://stackoverflow.com/)
