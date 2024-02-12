---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-27T20:37:08.150732-07:00
model:                 gpt-4-0125-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

TypeScript में यादृच्छिक संख्याएँ उत्पन्न करना एक निश्चित सीमा के भीतर अप्रत्याशित संख्यात्मक मूल्यों का निर्माण करने के बारे में है। प्रोग्रामर यूनिक पहचानकर्ता उत्पन्न करने, परीक्षण के लिए डेटा का अनुकरण करने, या खेलों और सिमुलेशनों में अप्रत्याशितता जोड़ने जैसे विभिन्न उद्देश्यों के लिए इन यादृच्छिक अंकों का लाभ उठाते हैं।

## कैसे:

TypeScript में, आप वैश्विक `Math` ऑब्जेक्ट का उपयोग करके यादृच्छिक संख्याएं उत्पन्न कर सकते हैं। नीचे विभिन्न आवश्यकताओं के लिए यादृच्छिक संख्याएं उत्पन्न करने के तरीके दिखाते हुए कुछ व्यावहारिक उदाहरण दिए गए हैं।

### एक साधारण यादृच्छिक संख्या उत्पन्न करना

0 (समावेशी) और 1 (विशिष्ट) के बीच एक साधारण यादृच्छिक दशमलव संख्या उत्पन्न करने के लिए, आप `Math.random()` का उपयोग करते हैं। इसके लिए किसी अतिरिक्त हेरफेर की आवश्यकता नहीं है:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

इससे `0.8995452185604771` जैसी मान आउटपुट हो सकती है।

### दो मूल्यों के बीच यादृच्छिक पूर्णांक उत्पन्न करना

जब आपको दो विशेष मूल्यों के बीच एक पूर्णांक की आवश्यकता होती है, तब आप `Math.random()` और कुछ अंकगणित को शामिल करते हैं:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

इससे 1 और 10 के बीच एक पूर्णांक मान उत्पन्न हो सकता है, जैसे `7`.

### एक यूनिक पहचानकर्ता उत्पन्न करना

यादृच्छिक संख्याओं को एक साधारण UUID जनरेटर स्निपेट के लिए अन्य विधियों के साथ मिलाया जा सकता है, उदाहरण के लिए:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

इससे `110e8400-e29b-41d4-a716-446655440000` जैसी एक UUID के समान स्ट्रिंग उत्पन्न होती है।

## गहराई से जानें

JavaScript और इस तरह TypeScript में यादृच्छिक संख्याएं उत्पन्न करने की प्राथमिक विधि, `Math.random()`, एक प्रीडो-रैंडम नंबर जनरेटर (PRNG) पर निर्भर करती है। यह ध्यान देना महत्वपूर्ण है कि जबकि परिणाम यादृच्छिक प्रतीत हो सकते हैं, वे एक प्रारंभिक बीज मूल्य पर आधारित एक निर्धारित एल्गोरिदम द्वारा उत्पन्न होते हैं। इसलिए, `Math.random()` द्वारा उत्पन्न संख्याएं वास्तव में यादृच्छिक नहीं हैं और क्रिप्टोग्राफिक उद्देश्यों के लिए इस्तेमाल नहीं की जानी चाहिए।

क्रिप्टोग्राफिक रूप से सुरक्षित यादृच्छिक संख्याओं के लिए, वेब क्रिप्टो API `crypto.getRandomValues()` प्रदान करता है, जो मॉडर्न ब्राउज़र्स और Node.js (क्रिप्टो मॉड्यूल के माध्यम से) सहित वेब क्रिप्टो मानक का समर्थन करने वाले वातावरणों में उपलब्ध है। यहाँ एक त्वरित उदाहरण है जो TypeScript में एक सीमा के भीतर एक सुरक्षित यादृच्छिक संख्या उत्पन्न करने के इसके उपयोग को दर्शाता है:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

यह विधि यादृच्छिकता का एक मजबूत स्तर प्रदान करती है और सुरक्षा-संवेदनशील अनुप्रयोगों के लिए अधिक उपयुक्त है। हालाँकि, यह अधिक संसाधन-गहन है और साधारण सिमुलेशनों या गैर-महत्वपूर्ण यादृच्छिक मूल्य उत्पादन जैसे कार्यों के लिए आवश्यक नहीं हो सकता है।