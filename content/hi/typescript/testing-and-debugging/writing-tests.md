---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:26.941499-07:00
description: "\u0915\u0948\u0938\u0947: TypeScript \u0905\u0927\u093F\u0915\u093E\u0902\
  \u0936 JavaScript \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0922\u093E\u0902\u091A\
  \u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0938\u0941\u0917\u092E\u0924\u093E\
  \u092A\u0942\u0930\u094D\u0935\u0915 \u0915\u093E\u092E \u0915\u0930\u0924\u093E\
  \ \u0939\u0948\u0964 \u092A\u094D\u0930\u0926\u0930\u094D\u0936\u0928 \u0915\u0947\
  \ \u0932\u093F\u090F, \u0939\u092E Jest, \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\
  \u0930\u093F\u092F \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u092B\u094D\u0930\
  \u0947\u092E\u0935\u0930\u094D\u0915 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \u2026"
lastmod: '2024-04-05T21:53:53.900839-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u0905\u0927\u093F\u0915\u093E\u0902\u0936 JavaScript \u092A\u0930\
  \u0940\u0915\u094D\u0937\u0923 \u0922\u093E\u0902\u091A\u094B\u0902 \u0915\u0947\
  \ \u0938\u093E\u0925 \u0938\u0941\u0917\u092E\u0924\u093E\u092A\u0942\u0930\u094D\
  \u0935\u0915 \u0915\u093E\u092E \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u0926\u0930\u094D\u0936\u0928 \u0915\u0947 \u0932\u093F\u090F, \u0939\
  \u092E Jest, \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u092A\
  \u0930\u0940\u0915\u094D\u0937\u0923 \u092B\u094D\u0930\u0947\u092E\u0935\u0930\u094D\
  \u0915 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902\u0917\
  \u0947, \u0907\u0938\u0915\u0940 TypeScript \u092A\u094D\u0930\u094B\u091C\u0947\
  \u0915\u094D\u091F\u094D\u0938 \u0915\u0947 \u0932\u093F\u090F \u0936\u0942\u0928\
  \u094D\u092F-\u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\u0947\u0936\u0928\
  \ \u0938\u0947\u091F\u0905\u092A \u0915\u0947 \u0915\u093E\u0930\u0923\u0964 \u0938\
  \u092C\u0938\u0947 \u092A\u0939\u0932\u0947, \u0938\u0941\u0928\u093F\u0936\u094D\
  \u091A\u093F\u0924 \u0915\u0930\u0947\u0902 \u0915\u093F \u0906\u092A\u0915\u0947\
  \ \u092A\u093E\u0938 Jest \u0914\u0930 \u0906\u0935\u0936\u094D\u092F\u0915 TypeScript\
  \ \u092A\u094D\u0930\u0915\u093E\u0930 \u0938\u094D\u0925\u093E\u092A\u093F\u0924\
  \ \u0939\u094B\u0902."
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

## कैसे:
TypeScript अधिकांश JavaScript परीक्षण ढांचों के साथ सुगमतापूर्वक काम करता है। प्रदर्शन के लिए, हम Jest, एक लोकप्रिय परीक्षण फ्रेमवर्क का उपयोग करेंगे, इसकी TypeScript प्रोजेक्ट्स के लिए शून्य-कॉन्फ़िगरेशन सेटअप के कारण।

सबसे पहले, सुनिश्चित करें कि आपके पास Jest और आवश्यक TypeScript प्रकार स्थापित हों:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

अगला, Jest को TypeScript के साथ काम करने के लिए सेटअप करें, `jest.config.js` को संशोधित करके या अगर एक नया बना रहे हैं तो:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

अब, एक सरल फ़ंक्शन और इसके लिए एक परीक्षण लिखें। `sum.ts` फ़ाइल के साथ निम्नलिखित फ़ंक्शन पर विचार करें:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

`sum.test.ts` नामक एक परीक्षण फ़ाइल बनाएँ:

```typescript
// sum.test.ts
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

अपने परीक्षणों को इसके साथ चलाएँ:

```bash
npx jest
```

संकेत-सारणी कि एक परीक्षण पास हो गया है कुछ इस तरह दिखाई देना चाहिए:

```plaintext
 PASS  ./sum.test.ts
  ✓ adds 1 + 2 to equal 3 (2 ms)
```

असिंक्रोनस कोड के लिए, Jest `async/await` के साथ सम्मिलित करता है। मान लीजिए कि आपके पास एक असिंक्रोनस `fetchData` फ़ंक्शन है:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

असिंक्रोनस फ़ंक्शन्स का उपयोग करते हुए आपका परीक्षण:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('fetches data successfully', async () => {
  expect(await fetchData()).toBe('data');
});
```

अपने परीक्षण चलाते समय, Jest प्रॉमिस के हल होने की प्रतीक्षा करेगा, सही ढंग से असिंक्रोनस ऑपरेशनों का परीक्षण करेगा।

याद रखें, प्रभावी परीक्षण में विभिन्न परिदृश्यों, सहित एज केसों के लिए कई परीक्षण लिखना शामिल है, ताकि यह सुनिश्चित किया जा सके कि आपका TypeScript कोड अपेक्षित रूप से व्यवहार करे।
