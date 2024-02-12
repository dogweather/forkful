---
title:                "एक टेक्स्ट फ़ाइल लिखना"
date:                  2024-02-03T19:30:48.226262-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
TypeScript में एक टेक्स्ट फाइल लिखना डेटा संरक्षण, कॉन्फ़िगरेशन, या लॉग जेनरेशन के लिए एक महत्वपूर्ण कौशल है। प्रोग्रामर अक्सर इस कार्य को डेटा विश्लेषण, रिपोर्टिंग, या केवल सत्रों भर में उपयोगकर्ता सेटिंग्स को बचाने जैसे कारणों के लिए एप्लिकेशन मेमोरी के बाहर डेटा संग्रहित और संभालने के लिए करते हैं।

## कैसे करें:
TypeScript स्वयं सीधे फ़ाइल ऑपरेशन्स को संभाल नहीं पाता क्योंकि यह JavaScript में कंपाइल होता है, जो पारंपरिक रूप से ब्राउज़र में चलता है जिसमें फ़ाइल सिस्टम तक सीमित पहुंच होती है। हालांकि, जब इसका उपयोग Node.js वातावरण में किया जाता है, तो `fs` मॉड्यूल (File System) फ़ाइलें लिखने की कार्यक्षमता प्रदान करता है।

### Node.js fs मॉड्यूल का प्रयोग 
पहले, सुनिश्चित करें कि आप Node.js वातावरण में काम कर रहे हैं। फिर, टेक्स्ट फाइलें लिखने के लिए `fs` मॉड्यूल का उपयोग करें। यहां एक बुनियादी उदाहरण है:

```typescript
import * as fs from 'fs';

const data = 'हेलो, वर्ल्ड!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('फ़ाइल सहेजी गई है!');
});
```

यह "हेलो, वर्ल्ड!" को `message.txt` में विलंबित रूप से लिखेगा। यदि फ़ाइल मौजूद नहीं है, तो Node.js इसे बना देता है; यदि यह है, तो Node.js इसे ओवरराइट कर देता है।

सिंक्रोनस फ़ाइल लिखने के लिए, `writeFileSync` का प्रयोग करें:

```typescript
import * as fs from 'fs';

const data = 'हेलो फिर से, वर्ल्ड!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('फ़ाइल सहेजी गई है!');
} catch (err) {
    console.error(err);
}
```

### लोकप्रिय थर्ड-पार्टी लाइब्रेरीज का प्रयोग
जबकि मूल `fs`मॉड्यूल शक्तिशाली है, कुछ डेवलपर्स अतिरिक्त सुविधा और कार्यक्षमता के लिए थर्ड-पार्टी लाइब्रेरीज का उपयोग करना पसंद करते हैं। `fs-extra` एक लोकप्रिय विकल्प है जो `fs` का विस्तार करता है और फ़ाइल ऑपरेशनों को अधिक सरल बना देता है।

सबसे पहले, आपको `fs-extra` इंस्टॉल करने की आवश्यकता होगी:

```
npm install fs-extra
```

फिर, आप इसे अपनी TypeScript फाइल में टेक्स्ट सामग्री लिखने के लिए उपयोग कर सकते हैं:

```typescript
import * as fs from 'fs-extra';

const data = 'यह fs-extra है!';
const filePath = './extraMessage.txt';

// एसिंक/अवेट का उपयोग करते हुए
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('फाइल fs-extra के साथ सहेजी गई है!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

यह कोड स्निपेट पहले के `fs` उदाहरणों की तरह ही काम करता है लेकिन `fs-extra` पुस्तकालय का उपयोग करता है, प्रॉमिसों को संभालने के लिए साफ़ सिंटैक्स पेश करता है।