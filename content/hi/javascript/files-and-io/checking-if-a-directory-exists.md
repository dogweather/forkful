---
title:                "डायरेक्टरी मौजूद है या नहीं जाँचना"
aliases: - /hi/javascript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:38.441605-07:00
model:                 gpt-4-0125-preview
simple_title:         "डायरेक्टरी मौजूद है या नहीं जाँचना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
