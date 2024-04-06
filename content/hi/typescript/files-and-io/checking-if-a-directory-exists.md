---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:27.689764-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: TypeScript \u0915\
  \u094B \u091C\u092C Node.js \u0935\u093E\u0924\u093E\u0935\u0930\u0923 \u092E\u0947\
  \u0902 \u091A\u0932\u093E\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948, \u0924\
  \u092C \u092F\u0939 \u0906\u092A\u0915\u094B `fs` \u092E\u0949\u0921\u094D\u092F\
  \u0942\u0932 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  \ \u092F\u0939 \u091C\u093E\u0902\u091A\u0928\u0947 \u0915\u0940 \u0905\u0928\u0941\
  \u092E\u0924\u093F \u0926\u0947\u0924\u093E \u0939\u0948 \u0915\u093F \u0915\u094B\
  \u0908 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0915\u093E \u092E\u094C\
  \u091C\u0942\u0926\u2026"
lastmod: '2024-04-05T21:53:53.919255-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u0915\u094B \u091C\u092C Node.js \u0935\u093E\u0924\u093E\u0935\
  \u0930\u0923 \u092E\u0947\u0902 \u091A\u0932\u093E\u092F\u093E \u091C\u093E\u0924\
  \u093E \u0939\u0948, \u0924\u092C \u092F\u0939 \u0906\u092A\u0915\u094B `fs` \u092E\
  \u0949\u0921\u094D\u092F\u0942\u0932 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0915\u0947 \u092F\u0939 \u091C\u093E\u0902\u091A\u0928\u0947 \u0915\
  \u0940 \u0905\u0928\u0941\u092E\u0924\u093F \u0926\u0947\u0924\u093E \u0939\u0948\
  \ \u0915\u093F \u0915\u094B\u0908 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\
  \u0915\u093E \u092E\u094C\u091C\u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\
  \u0940\u0902, \u091C\u094B `existsSync()` \u092B\u0902\u0915\u094D\u0936\u0928 \u092F\
  \u093E \u090F\u0938\u093F\u0902\u0915\u094D\u0930\u094B\u0928\u0938 `access()` \u092B\
  \u0902\u0915\u094D\u0936\u0928 \u0915\u094B `constants.F_OK` \u0915\u0947 \u0938\
  \u093E\u0925 \u0938\u0902\u092F\u0941\u0915\u094D\u0924 \u0915\u0930\u0915\u0947\
  \ \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964\
  ."
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902 \u091C\u093E\u0901\
  \u091A\u0928\u093E"
weight: 20
---

## कैसे करें:
TypeScript को जब Node.js वातावरण में चलाया जाता है, तब यह आपको `fs` मॉड्यूल का उपयोग करके यह जांचने की अनुमति देता है कि कोई निर्देशिका मौजूद है या नहीं, जो `existsSync()` फंक्शन या एसिंक्रोनस `access()` फंक्शन को `constants.F_OK` के साथ संयुक्त करके प्रदान करता है।

### `fs.existsSync()` का उपयोग करते हुए:
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('निर्देशिका मौजूद है।');
} else {
  console.log('निर्देशिका मौजूद नहीं है।');
}
```

### `fs.access()` का उपयोग `fs.constants.F_OK` के साथ करते हुए:
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('निर्देशिका मौजूद नहीं है।');
    return;
  }
  console.log('निर्देशिका मौजूद है।');
});
```

**दोनों तरीकों के लिए नमूना आउटपुट**, यह मानते हुए कि निर्देशिका मौजूद है:
```
निर्देशिका मौजूद है।
```

और अगर नहीं है तो:
```
निर्देशिका मौजूद नहीं है।
```

### तीसरे पक्ष की लाइब्रेरी - `fs-extra` का उपयोग करते हुए:
`fs-extra` एक लोकप्रिय तीसरे पक्ष की लाइब्रेरी है जो निर्मित `fs` मॉड्यूल को बढ़ाती है और अधिक सुविधाजनक फंक्शन्स प्रदान करती है।

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`निर्देशिका मौजूद है: ${exists}`);
});
```

**नमूना आउटपुट** जब निर्देशिका मौजूद होती है:
```
निर्देशिका मौजूद है: सही
```

और अगर नहीं है तो:
```
निर्देशिका मौजूद नहीं है: गलत
```
