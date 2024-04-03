---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:47.080174-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: TypeScript \u092E\
  \u0947\u0902 YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\
  \u0928\u093E \u0906\u092E\u0924\u094C\u0930 \u092A\u0930 YAML \u0938\u093E\u092E\
  \u0917\u094D\u0930\u0940 \u0915\u094B JavaScript \u0911\u092C\u094D\u091C\u0947\u0915\
  \u094D\u091F\u094D\u0938 \u092E\u0947\u0902 \u092A\u093E\u0930\u094D\u0938\u093F\
  \u0902\u0917, \u0914\u0930 \u0938\u0902\u092D\u0935 \u0939\u094B \u0924\u094B JavaScript\
  \ \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\u0938 \u0915\u094B \u0935\
  \u093E\u092A\u0938\u2026"
lastmod: '2024-03-13T22:44:51.924328-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u092E\u0947\u0902 YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\
  \u093E\u092E \u0915\u0930\u0928\u093E \u0906\u092E\u0924\u094C\u0930 \u092A\u0930\
  \ YAML \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B JavaScript \u0911\
  \u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\u0938 \u092E\u0947\u0902 \u092A\
  \u093E\u0930\u094D\u0938\u093F\u0902\u0917, \u0914\u0930 \u0938\u0902\u092D\u0935\
  \ \u0939\u094B \u0924\u094B JavaScript \u0911\u092C\u094D\u091C\u0947\u0915\u094D\
  \u091F\u094D\u0938 \u0915\u094B \u0935\u093E\u092A\u0938 YAML \u092E\u0947\u0902\
  \ \u092A\u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E\
  \ \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0964 \u0907\u0938\u0915\u0947 \u0932\
  \u093F\u090F \u090F\u0915 \u092A\u093E\u0930\u094D\u0938\u0930 \u0915\u0940 \u0906\
  \u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\u094B\u0924\u0940 \u0939\u0948\
  ; \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0935\u093F\u0915\
  \u0932\u094D\u092A `js-yaml` \u0939\u0948, \u090F\u0915 \u0932\u093E\u0907\u092C\
  \u094D\u0930\u0947\u0930\u0940 \u091C\u093F\u0938\u0947 TypeScript \u092A\u094D\u0930\
  \u094B\u091C\u0947\u0915\u094D\u091F\u094D\u0938 \u092E\u0947\u0902 \u0906\u0938\
  \u093E\u0928\u0940 \u0938\u0947 \u090F\u0915\u0940\u0915\u0943\u0924 \u0915\u093F\
  \u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964\n\n#."
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 41
---

## कैसे करें:
TypeScript में YAML के साथ काम करना आमतौर पर YAML सामग्री को JavaScript ऑब्जेक्ट्स में पार्सिंग, और संभव हो तो JavaScript ऑब्जेक्ट्स को वापस YAML में परिवर्तित करना शामिल है। इसके लिए एक पार्सर की आवश्यकता होती है; एक लोकप्रिय विकल्प `js-yaml` है, एक लाइब्रेरी जिसे TypeScript प्रोजेक्ट्स में आसानी से एकीकृत किया जा सकता है।

### js-yaml को इंस्टॉल करना
पहले, अपने प्रोजेक्ट में `js-yaml` जोड़ें:

```bash
npm install js-yaml
```

### YAML को JavaScript ऑब्जेक्ट में पार्सिंग
मान लीजिए आपके पास `config.yaml` नामक एक YAML फ़ाइल है जिसमें निम्नलिखित सामग्री है:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

आप इस फ़ाइल को निम्नलिखित रूप में पढ़ और पार्स कर सकते हैं ताकि यह एक JavaScript ऑब्जेक्ट बन जाए:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// YAML फ़ाइल को लोड और पार्स करें
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**नमूना आउटपुट:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### JavaScript ऑब्जेक्ट को YAML में परिवर्तित करना
यदि आपको दूसरी तरफ जाने की आवश्यकता है और एक JavaScript ऑब्जेक्ट को YAML स्ट्रिंग में परिवर्तित करने की आवश्यकता है, तो आप निम्नलिखित के रूप में `js-yaml` का उपयोग कर सकते हैं:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**नमूना आउटपुट:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

यह स्निपेट एक JavaScript ऑब्जेक्ट को YAML स्ट्रिंग में परिवर्तित करता है और इसे आउटपुट करता है। व्यवहार में, आप इसे वापस एक फ़ाइल में लिख सकते हैं या अपने एप्लीकेशन के अन्य हिस्सों में इसका उपयोग कर सकते हैं।
