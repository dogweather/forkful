---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:38.441605-07:00
description: "JavaScript \u092E\u0947\u0902 \u090F\u0915 \u0921\u093E\u092F\u0930\u0947\
  \u0915\u094D\u091F\u0930\u0940 \u0915\u0947 \u0905\u0938\u094D\u0924\u093F\u0924\
  \u094D\u0935 \u0915\u0940 \u091C\u093E\u0902\u091A \u0915\u0930\u0928\u093E \u092B\
  \u093E\u0907\u0932 \u0938\u0902\u091A\u093E\u0932\u0928 \u0915\u093E\u0930\u094D\
  \u092F\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0906\u0935\u0936\u094D\u092F\
  \u0915 \u0939\u0948, \u091C\u094B \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\
  \u091F\u094D\u0938 \u0915\u094B \u0909\u0938\u0938\u0947 \u092A\u0922\u093C\u0928\
  \u0947 \u092F\u093E \u0909\u0938\u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u0947\
  \ \u0938\u0947 \u092A\u0939\u0932\u0947 \u0921\u093E\u092F\u0930\u0947\u0915\u094D\
  \u091F\u0930\u0940\u2026"
lastmod: '2024-03-13T22:44:53.014554-06:00'
model: gpt-4-0125-preview
summary: "JavaScript \u092E\u0947\u0902 \u090F\u0915 \u0921\u093E\u092F\u0930\u0947\
  \u0915\u094D\u091F\u0930\u0940 \u0915\u0947 \u0905\u0938\u094D\u0924\u093F\u0924\
  \u094D\u0935 \u0915\u0940 \u091C\u093E\u0902\u091A \u0915\u0930\u0928\u093E \u092B\
  \u093E\u0907\u0932 \u0938\u0902\u091A\u093E\u0932\u0928 \u0915\u093E\u0930\u094D\
  \u092F\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0906\u0935\u0936\u094D\u092F\
  \u0915 \u0939\u0948, \u091C\u094B \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\
  \u091F\u094D\u0938 \u0915\u094B \u0909\u0938\u0938\u0947 \u092A\u0922\u093C\u0928\
  \u0947 \u092F\u093E \u0909\u0938\u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u0947\
  \ \u0938\u0947 \u092A\u0939\u0932\u0947 \u0921\u093E\u092F\u0930\u0947\u0915\u094D\
  \u091F\u0930\u0940\u2026"
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902 \u091C\u093E\u0901\
  \u091A\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
JavaScript में एक डायरेक्टरी के अस्तित्व की जांच करना फाइल संचालन कार्यों के लिए आवश्यक है, जो स्क्रिप्ट्स को उससे पढ़ने या उसमें लिखने से पहले डायरेक्टरी की उपस्थिति की पुष्टि करने में सक्षम बनाता है। यह क्रिया त्रुटियों को रोकती है और विशेषकर उन अप्लिकेशंस में कार्यक्रम का सुचारू संचालन सुनिश्चित करती है जो उपयोगकर्ता इनपुट या बाह्य डेटा स्रोतों के आधार पर फाइलों या डायरेक्टरीज़ को गतिशील रूप से संभालते हैं।

## कैसे:

Node.js में, चूंकि JavaScript स्वयं फाइल सिस्टम तक सीधी पहुंच नहीं रखता, इसलिए ऐसे संचालनों के लिए `fs` मॉड्यूल का आमतौर पर उपयोग किया जाता है। `fs.existsSync()` का उपयोग करके डायरेक्टरी के अस्तित्व की जांच करने का एक सरल तरीका यह है:

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// जांचें कि डायरेक्टरी मौजूद है या नहीं
if (fs.existsSync(directoryPath)) {
  console.log('डायरेक्टरी मौजूद है।');
} else {
  console.log('डायरेक्टरी मौजूद नहीं है।');
}
```
**नमूना आउटपुट:**
```
डायरेक्टरी मौजूद है।
```
या, गैर-बाधा रहित असिंक्रोनस दृष्टिकोण के लिए, `fs.promises` का उपयोग `async/await` के साथ करें:

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('डायरेक्टरी मौजूद है।');
  } catch (error) {
    console.log('डायरेक्टरी मौजूद नहीं है।');
  }
}

checkDirectory('./sample-directory');
```
**नमूना आउटपुट:**
```
डायरेक्टरी मौजूद है।
```

फाइल और डायरेक्टरी संचालनों का भारी उपयोग करने वाली परियोजनाओं के लिए, `fs-extra` पैकेज, मूल `fs` मॉड्यूल का एक विस्तार, सुविधाजनक अतिरिक्त तरीके उपलब्ध कराता है। `fs-extra` के साथ वही हासिल करने का एक तरीका यह है:

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// जांचें कि डायरेक्टरी मौजूद है या नहीं
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'डायरेक्टरी मौजूद है।' : 'डायरेक्टरी मौजूद नहीं है।'))
  .catch(err => console.error(err));
```
**नमूना आउटपुट:**
```
डायरेक्टरी मौजूद है।
```

यह दृष्टिकोण साफ, पठनीय कोड को सक्षम बनाता है जो आधुनिक JavaScript प्रक्रियाओं के साथ सहजता से एकीकृत करता है।
