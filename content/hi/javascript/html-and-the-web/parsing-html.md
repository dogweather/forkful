---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:01:10.007560-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u091A\u0932\u093F\
  \u090F `DOMParser` API \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\
  \u0947 \u0939\u0941\u090F JavaScript \u092E\u0947\u0902 HTML \u0915\u093E \u092A\
  \u093E\u0930\u094D\u0938 \u0915\u0930\u0947\u0902\u0964."
lastmod: '2024-03-13T22:44:52.986049-06:00'
model: gpt-4-0125-preview
summary: "\u091A\u0932\u093F\u090F `DOMParser` API \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F JavaScript \u092E\u0947\
  \u0902 HTML \u0915\u093E \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0947\u0902\
  \u0964."
title: "HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 43
---

## कैसे करें:
चलिए `DOMParser` API का उपयोग करते हुए JavaScript में HTML का पार्स करें।

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // आउटपुट: Hello, world!
```

अब, चलिए कुछ और विशिष्ट चीज़ को पकड़ते हैं, जैसे एक क्लास के साथ एक तत्त्व:

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // आउटपुट: Hello, again!
```

## गहराई से जानकारी
HTML का पार्सिंग वेब जितना पुराना है। शुरू में, यह एक ब्राउज़र चीज थी—ब्राउज़र्स HTML का पार्सिंग करते थे ताकि वेब पृष्ठों को प्रदर्शित किया जा सके। समय के साथ, प्रोग्रामर इस प्रक्रिया में दखल देना चाहते थे, जिससे `DOMParser` जैसे API तक पहुँच प्राप्त हुई।

विकल्प? बिलकुल। हमारे पास `jQuery` जैसी लाइब्रेरीज और `BeautifulSoup` जैसे उपकरण Python के लिए हैं। लेकिन JavaScript का मूल `DOMParser` तेज और बिना किसी अतिरिक्त लाइब्रेरी के मौजूद है।

कार्यान्वयन के दृष्टिकोण से, जब आप `DOMParser` के साथ HTML का पार्सिंग करते हैं, यह एक `Document` ऑब्जेक्ट बनाता है। इसे अपने HTML का एक पदानुक्रमिक मॉडल समझें। एक बार जब आप इसे प्राप्त कर लेते हैं, तो आप इसे नेविगेट और हेरफेर कर सकते हैं, ठीक उसी तरह जैसे आप एक सामान्य वेब पृष्ठ के DOM के साथ करेंगे।

यहां बात यह है—पार्सिंग खराब बनाई गई HTML पर ठोकर खा सकती है। ब्राउज़र्स क्षमाशील होते हैं, लेकिन `DOMParser` नहीं हो सकता। इसलिए, जटिल कार्यों या गड़बड़ी वाले HTML के लिए, तीसरे पक्ष की लाइब्रेरी बेहतर सफाई कार्य कर सकती है।

## साथ में देखें
- `DOMParser` API पर MDN वेब डॉक्स: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQuery की पार्सिंग क्षमताएं: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, सर्वर के लिए कोर jQuery का एक तेज़, लचीला और लीन कार्यान्वयन: [Cheerio.js](https://cheerio.js.org/)
- गैर-JS पार्सिंग के लिए: Python की BeautifulSoup लाइब्रेरी: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
