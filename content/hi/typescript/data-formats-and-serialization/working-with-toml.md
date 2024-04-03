---
date: 2024-01-26 04:28:01.472786-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0938\u092C\u0938\
  \u0947 \u092A\u0939\u0932\u0947, \u0906\u092A\u0915\u094B \u090F\u0915 TOML \u092A\
  \u093E\u0930\u094D\u0938\u0930 \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\
  \u0924\u093E \u0939\u094B\u0917\u0940\u0964 `@iarna/toml` \u090F\u0915 \u0932\u094B\
  \u0915\u092A\u094D\u0930\u093F\u092F \u0935\u093F\u0915\u0932\u094D\u092A \u0939\
  \u0948\u0964 \u0907\u0938\u0947 npm \u0915\u0947 \u0938\u093E\u0925 \u0938\u094D\
  \u0925\u093E\u092A\u093F\u0924 \u0915\u0930\u0947\u0902: `npm install @iarna/toml\u2026"
lastmod: '2024-03-13T22:44:51.929204-06:00'
model: gpt-4-0125-preview
summary: "\u0938\u092C\u0938\u0947 \u092A\u0939\u0932\u0947, \u0906\u092A\u0915\u094B\
  \ \u090F\u0915 TOML \u092A\u093E\u0930\u094D\u0938\u0930 \u0915\u0940 \u0906\u0935\
  \u0936\u094D\u092F\u0915\u0924\u093E \u0939\u094B\u0917\u0940\u0964 `@iarna/toml`\
  \ \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0935\u093F\u0915\
  \u0932\u094D\u092A \u0939\u0948\u0964 \u0907\u0938\u0947 npm \u0915\u0947 \u0938\
  \u093E\u0925 \u0938\u094D\u0925\u093E\u092A\u093F\u0924 \u0915\u0930\u0947\u0902\
  ."
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 39
---

## कैसे करें:
सबसे पहले, आपको एक TOML पार्सर की आवश्यकता होगी। `@iarna/toml` एक लोकप्रिय विकल्प है। इसे npm के साथ स्थापित करें: `npm install @iarna/toml --save`। यहाँ एक TOML फ़ाइल को पढ़ने और इसे जावास्क्रिप्ट ऑब्जेक्ट में पार्स करने का तरीका है:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
अगर `config.toml` में होता है:
```
[server]
port = 8080
```
आउटपुट होगा:
```
{ server: { port: 8080 } }
```
और, एक TOML फ़ाइल में लिखना उतना ही सरल है:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
यह कोड चलाने से `config.toml` में TOML प्रारूप में ऑब्जेक्ट लिखा जाता है।

## गहराई से समझें
TOML का निर्माण GitHub के सह-संस्थापक टॉम प्रेस्टन-वर्नर ने 2013 के आसपास अन्य प्रारूपों जैसे कि INI या YAML में उन्होंने जो सीमाएँ देखीं, उनके जवाब में किया था। इसे डेटा संरचनाओं में आसानी से पार्स करने के लिए अस्पष्टता रहित और सरल डिजाइन किया गया है, इसलिए, कॉन्फिग फ

ाइलों के लिए एक पसंदीदा। उदाहरण के लिए JSON में टिप्पणियाँ नहीं हैं, जबकि YAML अधिक जटिल है। TOML अपनी सरलता और जटिल डेटा हायरार्कियों को स्पष्ट रूप से प्रस्तुत करने की क्षमता में चमकता है। 

अंतराल में, जब आप TypeScript में TOML पार्स करते हैं, तो आप मूल टेक्स्ट को भाषा द्वारा संचालित करने योग्य एक संरचित प्रारूप में परिवर्तित कर रहे हैं। इसमें लेक्सिंग (कच्चे टेक्स्ट को टोकन में बदलना) और पार्सिंग (एक आंतरिक डेटा संरचना बनाना) शामिल है; `@iarna/toml` दोनों को सहजता से संभालता है। इमोजी सपोर्ट TOML के उपयोगकर्ता-केंद्रित दृष्टिकोण को दिखाने वाला एक मजेदार स्पर्श है।

## देखें भी
- TOML आधिकारिक स्पेक: https://toml.io/en/
- `@iarna/toml` पैकेज: https://www.npmjs.com/package/@iarna/toml
- TOML, YAML, और JSON के बीच तुलनाएँ: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
