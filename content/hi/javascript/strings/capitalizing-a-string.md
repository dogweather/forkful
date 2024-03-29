---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:35.217988-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\
  \u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C \u0915\u0930\u0928\u0947\
  \ \u0915\u093E \u0905\u0930\u094D\u0925 \u0939\u0948 \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u0915\u0947 \u092A\u0939\u0932\u0947 \u0905\u0915\u094D\u0937\
  \u0930 \u0915\u094B \u092C\u0921\u093C\u0947 \u0905\u0915\u094D\u0937\u0930 (\u0905\
  \u092A\u0930\u0915\u0947\u0938) \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E\
  \ \u091C\u092C\u0915\u093F \u0936\u0947\u0937 \u0905\u0915\u094D\u0937\u0930\u094B\
  \u0902 \u0915\u094B \u091C\u0948\u0938\u0947 \u0915\u093E \u0924\u0948\u0938\u093E\
  \ \u0930\u0916\u0928\u093E\u0964 \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\
  \u0930\u093F\u092A\u094D\u091F \u092E\u0947\u0902\u2026"
lastmod: '2024-03-13T22:44:52.961851-06:00'
model: gpt-4-0125-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C\u093C \u0915\u0930\u0928\u0947 \u0915\
  \u093E \u0905\u0930\u094D\u0925 \u0939\u0948 \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917 \u0915\u0947 \u092A\u0939\u0932\u0947 \u0905\u0915\u094D\u0937\u0930\
  \ \u0915\u094B \u092C\u0921\u093C\u0947 \u0905\u0915\u094D\u0937\u0930 (\u0905\u092A\
  \u0930\u0915\u0947\u0938) \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E \u091C\
  \u092C\u0915\u093F \u0936\u0947\u0937 \u0905\u0915\u094D\u0937\u0930\u094B\u0902\
  \ \u0915\u094B \u091C\u0948\u0938\u0947 \u0915\u093E \u0924\u0948\u0938\u093E \u0930\
  \u0916\u0928\u093E\u0964 \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\
  \u092A\u094D\u091F \u092E\u0947\u0902\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग को कैपिटलाइज़ करने का अर्थ है स्ट्रिंग के पहले अक्षर को बड़े अक्षर (अपरकेस) में बदलना जबकि शेष अक्षरों को जैसे का तैसा रखना। जावास्क्रिप्ट में उपयोगकर्ता इनपुट्स को फॉर्मेट करने, नामों या शीर्षकों को प्रदर्शित करने, और उपयोगकर्ता इंटरफेस टेक्स्ट्स में संगति सुनिश्चित करने के लिए यह क्रिया आमतौर पर की जाती है।

## कैसे करें:
जावास्क्रिप्ट में, सीधे स्ट्रिंग्स को कैपिटलाइज़ करने के लिए कोई बिल्ट-इन मेथड नहीं है, लेकिन मूल स्ट्रिंग मैनिपुलेशन मेथड्स का उपयोग करके इसे लागू करना सरल है।

### मानक जावास्क्रिप्ट का उपयोग करते हुए
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // आउटपुट: "Hello world"
```

### ES6 संस्करण
ES6 टेम्पलेट लिटरल्स के साथ, फंक्शन को अधिक संक्षिप्त तरीके से लिखा जा सकता है:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // आउटपुट: "Hello ES6"
```

### Lodash का उपयोग करते हुए
Lodash एक लोकप्रिय तृतीय-पक्ष यूटिलिटी लाइब्रेरी है जो जावास्क्रिप्ट मूल्यों, सहित स्ट्रिंग्स के साथ कार्य करने और उन्हें मैनिपुलेट करने के लिए विस्तृत रेंज के फंक्शंस प्रदान करती है। Lodash का उपयोग करके स्ट्रिंग को कैपिटलाइज़ करने के लिए:
```javascript
// पहले, यदि आपने नहीं किया है तो lodash इंस्टॉल करें: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // आउटपुट: "Lodash example"
```
_ध्यान दें कि Lodash न केवल पहले अक्षर को कैपिटलाइज़ करता है, बल्कि शेष स्ट्रिंग को लोअर केस में भी बदल देता है, जो कि सादे जावास्क्रिप्ट लागू करने से थोड़ा अलग है।_

### CSS का उपयोग करते हुए (केवल प्रदर्शन उद्देश्यों के लिए)
यदि लक्ष्य UI में टेक्स्ट को कैपिटलाइज़ करके प्रदर्शित करना है, तो CSS का उपयोग किया जा सकता है:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- "Hello css" के रूप में प्रदर्शित होता है -->
```
**नोट:** यह विधि जावास्क्रिप्ट में स्ट्रिंग को बदले बिना वेबपेज पर टेक्स्ट कैसे दिखाई देता है इसमें परिवर्तन करती है।
