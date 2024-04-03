---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:38.682419-07:00
description: "JSON (JavaScript Object Notation) \u0915\u0947 \u0938\u093E\u0925 \u0915\
  \u093E\u092E \u0915\u0930\u0928\u093E TypeScript \u092E\u0947\u0902 \u0909\u092A\
  \u092F\u094B\u0917\u0940 \u092A\u094D\u0930\u093E\u0930\u0942\u092A \u092E\u0947\
  \u0902 JSON \u0921\u0947\u091F\u093E \u0915\u094B \u092A\u093E\u0930\u094D\u0936\
  \ \u0915\u0930\u0928\u0947 \u0914\u0930 \u0909\u0938\u0938\u0947 \u092A\u093E\u0930\
  \u094D\u0936 \u0915\u0930\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\
  \u0930\u093F\u092F\u093E \u0939\u094B\u0924\u0940 \u0939\u0948\u0964\u2026"
lastmod: '2024-03-13T22:44:51.925957-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u0915\u0947 \u0938\u093E\u0925 \u0915\
  \u093E\u092E \u0915\u0930\u0928\u093E TypeScript \u092E\u0947\u0902 \u0909\u092A\
  \u092F\u094B\u0917\u0940 \u092A\u094D\u0930\u093E\u0930\u0942\u092A \u092E\u0947\
  \u0902 JSON \u0921\u0947\u091F\u093E \u0915\u094B \u092A\u093E\u0930\u094D\u0936\
  \ \u0915\u0930\u0928\u0947 \u0914\u0930 \u0909\u0938\u0938\u0947 \u092A\u093E\u0930\
  \u094D\u0936 \u0915\u0930\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\
  \u0930\u093F\u092F\u093E \u0939\u094B\u0924\u0940 \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0938\u0902\
  \u0930\u091A\u093F\u0924 \u0921\u0947\u091F\u093E \u0915\u094B \u0906\u0938\u093E\
  \u0928\u0940 \u0938\u0947 \u0938\u0902\u092D\u093E\u0932\u0928\u0947, \u0938\u0902\
  \u0917\u094D\u0930\u0939\u093F\u0924 \u0915\u0930\u0928\u0947 \u092F\u093E \u092A\
  \u094D\u0930\u0947\u0937\u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902, \u0915\u094D\u092F\u094B\
  \u0902\u0915\u093F JSON \u0939\u0932\u094D\u0915\u093E, \u092A\u093E\u0920-\u0906\
  \u0927\u093E\u0930\u093F\u0924 \u0939\u094B\u0924\u093E \u0939\u0948, \u0914\u0930\
  \ \u0907\u0902\u0938\u093E\u0928 \u0914\u0930 \u092E\u0936\u0940\u0928\u0947\u0902\
  \ \u0926\u094B\u0928\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0906\u0938\u093E\
  \u0928\u0940 \u0938\u0947 \u092A\u0922\u093C\u0928\u0947 \u092F\u094B\u0917\u094D\
  \u092F \u0939\u094B\u0924\u093E \u0939\u0948\u0964."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे:


### JSON को एक TypeScript ऑब्जेक्ट में पार्स करना
एक JSON स्ट्रिंग को TypeScript ऑब्जेक्ट में बदलने के लिए, आप `JSON.parse()` विधि का उपयोग करते हैं। जब आप वेब सर्वर से JSON डेटा प्राप्त कर रहे होते हैं या एक JSON फाइल पढ़ रहे होते हैं तो यह उपयोगी होता है।

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // आउटपुट: John Doe
```

### एक TypeScript ऑब्जेक्ट को JSON में स्ट्रिंगिफाई करना
एक TypeScript ऑब्जेक्ट को JSON स्ट्रिंग में बदलने के लिए, आप `JSON.stringify()` विधि का उपयोग करते हैं। यह विशेष रूप से उपयोगी होता है जब आपको वेब सर्वर को डेटा भेजना होता है।

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // आउटपुट: {"name":"Jane Doe","age":25}
```

### इंटरफेसेज के साथ काम करना
आप TypeScript इंटरफेसेज़ को परिभाषित करके JSON डेटा के साथ सहजता से काम कर सकते हैं, जिससे आपके ऑब्जेक्ट्स की संरचना सुनिश्चित होती है।

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // आउटपुट: 28
```

### लोकप्रिय तृतीय-पक्ष पुस्तकालयों का उपयोग करना
अधिक जटिल परिदृश्यों के लिए, जैसे कि स्कीमा मान्यकरण या रूपांतरण, आप `class-transformer` या `AJV` (Another JSON Schema Validator) जैसी पुस्तकालयों का सहारा ले सकते हैं।

#### class-transformer
यह पुस्तकालय सादे ऑब्जेक्ट्स को वर्ग उदाहरण में और विपरीत में परिवर्तित करने में सहायक होता है, जो प्रकार जाँच और हेरफेर के लिए उपयोगी है।

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // आउटपुट: true
console.log(person.name); // आउटपुट: Mia
```

#### AJV
AJV एक पुस्तकालय है जो तेजी से JSON स्कीमा मान्यकरण की अनुमति देता है। इसका मतलब है कि आप पूर्वनिर्धारित स्कीमाओं के विरुद्ध JSON ऑब्जेक्ट्स का मान्यकरण कर सकते हैं।

```typescript
import Ajv from "ajv";

const ajv = new Ajv();

const schema = {
  type: "object",
  properties: {
    name: { type: "string" },
    age: { type: "number" },
  },
  required: ["name", "age"],
  additionalProperties: false,
};

const validate = ajv.compile(schema);
const valid = validate({ name: "Tom", age: 24 });

console.log(valid); // आउटपुट: true
if (!valid) console.log(validate.errors);
```

इन उपकरणों और तकनीकों के साथ, आप अपने TypeScript अनुप्रयोगों में JSON डेटा को कुशलतापूर्वक संभाल सकते हैं, डेटा अखंडता सुनिश्चित कर सकते हैं और TypeScript की शक्तिशाली प्रकार प्रणाली का लाभ उठा सकते हैं।
