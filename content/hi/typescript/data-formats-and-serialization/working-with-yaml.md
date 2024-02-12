---
title:                "YAML के साथ काम करना"
aliases: - /hi/typescript/working-with-yaml.md
date:                  2024-02-03T19:27:47.080174-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML, एक डेटा सीरियलिजेशन भाषा जिसे मनुष्य-मित्रता के साथ डिज़ाइन किया गया है, अक्सर कॉन्फ़िगरेशन फ़ाइलों, इंटर-प्रॉसेस मेसेजिंग, और डेटा स्टोरेज के लिए इस्तेमाल किया जाता है। प्रोग्रामर YAML का उपयोग इसकी पठनीयता और उपयोग में आसानी के कारण करते हैं, खासकर जब जटिल संरचित डेटा के साथ डील करते समय, जो इसे TypeScript में विकसित किए गए एप्लीकेशनों के लिए एक उत्कृष्ट विकल्प बनाता है।

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
