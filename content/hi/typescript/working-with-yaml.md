---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML एक डाटा सिरियलाइजेशन फॉर्मेट है जो JSON का सरलीकृत रूप है, और यह पठनीयता में उत्कृष्ट है। प्रोग्रामर इसे कॉन्फ़िगरेशन फ़ाइल्स, डाटा स्टोरेज और कम्यूनिकेशन बीच सर्विसेज़ के लिए इस्तेमाल करते हैं क्योंकि यह मानव और मशीन दोनों के लिए समझने में आसान होता है।

## How to: (कैसे करें:)
TypeScript में YAML के साथ काम करने के लिए, हम `js-yaml` लाइब्रेरी का इस्तेमाल कर सकते हैं। नीचे एक साधारण उदाहरण दिया गया है:

```typescript
import * as yaml from 'js-yaml';
import * as fs from 'fs';

// YAML फाइल को पढ़ना और JS ऑब्जेक्ट में परिवर्तित करना
const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
console.log(doc);

// JS ऑब्जेक्ट को YAML स्ट्रिंग में परिवर्तित करना
const yamlStr = yaml.dump(doc);
console.log(yamlStr);
```
सैंपल `config.yaml`:
```yaml
version: 1
services:
  - web: 
      image: "my-web-app:latest"
  - database:
      image: "mysql:5.7"
      environment:
        MYSQL_ROOT_PASSWORD: "s3cret"
```
संभावित आउटपुट:
```
{ version: 1, services: [ {...}, {...} ] }
```

## Deep Dive (गहराई में जानकारी)
YAML, जिसे "YAML Ain't Markup Language" भी कहा जाता है, 2001 में विकसित किया गया था। यह JSON की तुलना में अधिक पठनीय है लेकिन कम व्यापक है। इसके विकल्पों में JSON और XML शामिल हैं। YAML का उपयोग करते समय आपको इंडेंटेशन पर ध्यान देना होता है क्योंकि यह स्ट्रक्चर को प्रभावित करता है। वर्तमान कंटेनराइजेशन और ऑर्केस्ट्रेशन सिस्टम्स जैसे कुबेरनेटीस YAML के लिए भारी निर्भरता बढ़ा रहे हैं।

## See Also (यह भी देखें)
- YAML ऑफिशल स्पेसिफिकेशन: https://yaml.org/spec.html
- `js-yaml` NPM पैकेज: https://www.npmjs.com/package/js-yaml
- YAML और JSON की तुलना: https://json2yaml.com/
- YAML वैलिडेटर: http://www.yamllint.com/
