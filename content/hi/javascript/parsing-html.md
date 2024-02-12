---
title:                "HTML पार्स करना"
aliases:
- hi/javascript/parsing-html.md
date:                  2024-01-28T03:01:10.007560-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML पार्स करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/parsing-html.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML का पार्सिंग का मतलब है HTML दस्तावेज़ से डेटा निकालना। प्रोग्रामर इसे वेब सामग्री के साथ इंटरैक्ट या हेरफेर करने, डेटा निकासी को स्वचालित करने, या वेब स्क्रेपिंग के उद्देश्यों के लिए करते हैं।

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
