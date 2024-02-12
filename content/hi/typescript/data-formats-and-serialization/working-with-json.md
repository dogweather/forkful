---
title:                "JSON के साथ काम करना"
date:                  2024-02-03T19:25:38.682419-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON (JavaScript Object Notation) के साथ काम करना TypeScript में उपयोगी प्रारूप में JSON डेटा को पार्श करने और उससे पार्श करने की प्रक्रिया होती है। प्रोग्रामर इसे संरचित डेटा को आसानी से संभालने, संग्रहित करने या प्रेषित करने के लिए करते हैं, क्योंकि JSON हल्का, पाठ-आधारित होता है, और इंसान और मशीनें दोनों के लिए आसानी से पढ़ने योग्य होता है।

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
