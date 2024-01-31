---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
simple_title:         "यामल के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML एक डेटा सीरियलाइज़ेशन भाषा है, जिसे आसानी से पढ़ा और लिखा जा सकता है। जावास्क्रिप्ट प्रोग्रामर JSON के विकल्प के रूप में YAML का उपयोग कॉन्फ़िगरेशन फाइल्स, डेटा इंटरचेंज, और डेटा स्टोरेज के लिए करते हैं।

## How to (कैसे करें):
निम्नलिखित JavaScript कोड `js-yaml` पैकेज की मदद से YAML फाइल को पढ़ता और पर्स करता है:

```Javascript
const yaml = require('js-yaml');
const fs = require('fs');

try {
  const config = yaml.load(fs.readFileSync('config.yaml', 'utf8'));
  console.log(config);
} catch (e) {
console.error(e);
}
```
`config.yaml` फाइल इस प्रकार हो सकती है:

```yaml
version: 1
services:
  webapp:
    image: "my-web-app:latest"
    ports:
      - "8080:80"
```

सैंपल आउटपुट:

```Javascript
{ version: 1, services: { webapp: { image: 'my-web-app:latest', ports: [ '8080:80' ] } } }
```

## Deep Dive (गहराई से जानकारी):
YAML ("YAML Ain't Markup Language" के लिए) 2001 में विकसित की गई थी, ताकि XML का एक सरल विकल्प प्रस्तुत किया जा सके। जावास्क्रिप्ट में YAML के विकल्प JSON और XML हैं, लेकिन YAML मानव-पठनीयता में उत्कृष्ट है। उसका उपयोग `js-yaml` जैसे पैकेज के साथ सरल होता है, जो Node.js में YAML को जावास्क्रिप्ट ऑब्जेक्ट में बदलने में सहायता करता है।

## See Also (और देखें):
- YAML ऑफिशियल वेबसाइट: [https://yaml.org/](https://yaml.org/)
- `js-yaml` NPM पैकेज: [https://www.npmjs.com/package/js-yaml](https://www.npmjs.com/package/js-yaml)
- JSON और YAML की तुलना: [https://www.json2yaml.com/](https://www.json2yaml.com/)

इस लेख से आपको जावास्क्रिप्ट में YAML काम करने की बुनियादी जानकारी व मदद मिली होगी।
