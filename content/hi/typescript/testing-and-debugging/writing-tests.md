---
title:                "टेस्ट लिखना"
aliases:
- /hi/typescript/writing-tests/
date:                  2024-02-03T19:33:26.941499-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
TypeScript में परीक्षण लिखना आपके कोड की कार्यक्षमता और सहीता की जांच के लिए स्वचालित स्क्रिप्ट्स बनाने का काम शामिल है। प्रोग्रामर इसे विश्वसनीयता सुनिश्चित करने, जल्दी से बग्स पकड़ने, और रखरखाव योग्य कोड वृद्धि को सुविधा प्रदान करने के लिए करते हैं, क्योंकि TypeScript की स्टैटिक टाइपिंग JavaScript परीक्षण के लिए एक स्तर की पूर्वाह्नता जोड़ती है।

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
