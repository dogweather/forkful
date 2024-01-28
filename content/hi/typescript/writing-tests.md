---
title:                "परीक्षण लिखना"
date:                  2024-01-19
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
परीक्षण लिखने का मतलब कोड के छोटे टुकड़ों का परीक्षण करना है ताकि उनके बिना किसी गड़बड़ी के सही ढंग से काम करने की पुष्टि हो सके। प्रोग्रामर इसलिए परीक्षण लिखते हैं ताकि बग्स से बचा जा सके और सॉफ्टवेयर की गुणवत्ता सुनिश्चित की जा सके।

## कैसे करें:
```TypeScript
import { assertEquals } from 'https://deno.land/std/testing/asserts.ts';

function sum(a: number, b: number): number {
  return a + b;
}

Deno.test('sum कार्य का परीक्षण', () => {
  assertEquals(sum(1, 2), 3);
});

// Sample Output: 
// test sum कार्य का परीक्षण ... ok (1ms)
```

## गहराई से:
परीक्षण लिखने का अभ्यास 1960 के दशक से चला आ रहा है। पहले मैन्युअल परीक्षण अधिक थे पर अब ऑटोमेटेड टेस्टिंग का चलन है। विकल्पों में जेस्ट(Jest), मोचा(Mocha) और जैस्मीन(Jasmine) शामिल हैं। TypeScript में टेस्ट्स लिखते समय देखना होता है कि टाइप सेफ्टी ठीक से काम कर रही है या नहीं।

## और भी:
- टाइपस्क्रिप्ट के साथ जेस्ट का प्रयोग: [Jest with TypeScript](https://jestjs.io/docs/en/getting-started#using-typescript)
- मोचा का परिचय: [Mocha - the fun, simple, flexible JavaScript test framework](https://mochajs.org/)
