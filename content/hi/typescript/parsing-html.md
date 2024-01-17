---
title:                "HTML को विश्लेषण करना"
html_title:           "TypeScript: HTML को विश्लेषण करना"
simple_title:         "HTML को विश्लेषण करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/parsing-html.md"
---

{{< edit_this_page >}}

# पार्सिंग HTML क्या होता है और क्यों किया जाता है?
पार्सिंग HTML का मतलब है HTML को समझना और उसमें से डेटा को अलग करना। वह कंप्यूटर भाषा होती है जिसका उपयोग वेब पेज्स को बनाने और प्रदर्शित करने के लिए होता है। पार्सिंग एक अहम् काम है क्यूंकि वह आमतौर पर अवधारणा होता है की कैसे कोड को समझा जाना चाहिए।

# कैसे करें:
```TypeScript
const html = `<!DOCTYPE html>
<html>
<head>
<title>My Website</title>
</head>
<body>
<h1>Welcome to my website!</h1>
<p>This is a sample website made with HTML.</h1>
</body>
</html>`;

const parser = new DOMParser();
const doc = parser.parseFromString(html, 'text/html');

console.log(doc.title); // Output: My Website
console.log(doc.body.innerHTML); // Output: <h1>Welcome to my website!</h1> <p>This is a sample website made with HTML.</h1>
```

# गहराई में पढ़ें:
पार्सिंग HTML की शुरुआत 1993 में डीएमके (वर्ल्ड वाइड वेब) के संस्थान (CERN) द्वारा स्टेव बर्स और टिम बर्नर्स आदि द्वारा प्रयोजनों को संचित करने के लिए की गई। विकल्प हैं जो पार्सिंग HTML की जगह काम करते हैं, जैसे की एक्सएमएल बेस संस्थापित, साथ ही एक दूसरे प्रारंभिक पंजीयन संस्थापित। पार्सिंग HTML की विस्तृत जानकारी के लिए, फ़ायरफॉक्स, गूगल क्रोम और इंटरनेट एक्स्प्लोरर जैसे ब्राउज़र के उपयोग की अवधारणा को समझने में मदद की जा सकती है।

# देखें भी:
- [MDN डॉक्यूमेंटेशन - HTML पार्सिंग](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)
- [पार्सिंग HTML के बारे में अधिक जानकारी के लिए कार्नेल विज्ञान ब्लॉग](https://www.kernel.org/blogs/)