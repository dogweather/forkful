---
date: 2024-01-26 00:55:18.732187-07:00
description: "\u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\
  \u091F \u092E\u0947\u0902 \u0924\u094D\u0930\u0941\u091F\u093F \u0938\u0902\u091A\
  \u093E\u0932\u0928 (Error Handling) \u0906\u092A\u0915\u0947 \u0915\u094B\u0921\
  \ \u092E\u0947\u0902 \u0905\u092A\u094D\u0930\u0924\u094D\u092F\u093E\u0936\u093F\
  \u0924 \u092A\u0930\u093F\u0938\u094D\u0925\u093F\u0924\u093F\u092F\u094B\u0902\
  \ \u0915\u093E \u092A\u094D\u0930\u092C\u0902\u0927\u0928 \u0915\u0948\u0938\u0947\
  \ \u0915\u0930\u0947\u0902, \u0907\u0938\u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\
  \u0947\u0902 \u0939\u0948\u0964 \u092F\u0939 \u092C\u0939\u0941\u0924 \u092E\u0939\
  \u0924\u094D\u0935\u092A\u0942\u0930\u094D\u0923 \u0939\u0948\u2026"
lastmod: '2024-03-13T22:44:53.002400-06:00'
model: gpt-4-1106-preview
summary: "\u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\
  \u091F \u092E\u0947\u0902 \u0924\u094D\u0930\u0941\u091F\u093F \u0938\u0902\u091A\
  \u093E\u0932\u0928 (Error Handling) \u0906\u092A\u0915\u0947 \u0915\u094B\u0921\
  \ \u092E\u0947\u0902 \u0905\u092A\u094D\u0930\u0924\u094D\u092F\u093E\u0936\u093F\
  \u0924 \u092A\u0930\u093F\u0938\u094D\u0925\u093F\u0924\u093F\u092F\u094B\u0902\
  \ \u0915\u093E \u092A\u094D\u0930\u092C\u0902\u0927\u0928 \u0915\u0948\u0938\u0947\
  \ \u0915\u0930\u0947\u0902, \u0907\u0938\u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\
  \u0947\u0902 \u0939\u0948\u0964 \u092F\u0939 \u092C\u0939\u0941\u0924 \u092E\u0939\
  \u0924\u094D\u0935\u092A\u0942\u0930\u094D\u0923 \u0939\u0948\u2026"
title: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

जावास्क्रिप्ट में त्रुटि संचालन (Error Handling) आपके कोड में अप्रत्याशित परिस्थितियों का प्रबंधन कैसे करें, इसके बारे में है। यह बहुत महत्वपूर्ण है क्योंकि यह आपके कार्यक्रमों को सुगमतापूर्वक असफल होने में मदद करता है और उपयोगकर्ताओं को स्पष्ट निर्देश प्रदान करता है, बजाय सिर्फ गिरने और जलने के।

## कैसे करें:

यहाँ क्लासिक `try-catch` ब्लॉक है:

```javascript
try {
  // कोड जो त्रुटि फेंक सकता है
  let result = potentiallyRiskyOperation();
  console.log('Success:', result);
} catch (error) {
  // जब त्रुटि फेंकी जाए, तब क्या करना है
  console.error('Oops:', error.message);
}
```

जब कोई त्रुटि न हो तो नमूना आउटपुट:
```
Success: 42
```

और जब त्रुटि हो:
```
Oops: कुछ गलत हुआ
```

असिंक्रोनस कोड के लिए, जहाँ प्रॉमिसेज शामिल होते हैं, `try-catch` का उपयोग `async` फंक्शन में करें:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Data fetched:', data);
  } catch (error) {
    console.error('Error fetching data:', error.message);
  }
}

fetchData();
```

## गहराई से विचार:

जावास्क्रिप्ट में त्रुटि संचालन समय के साथ विकसित हुआ है। पुराने दिनों में (ES3, लगभग 1999), हमारे पास सिर्फ `try-catch` ब्लॉक था। बहुत लचीला नहीं, लेकिन काम करता था।

ES6 (2015) में प्रॉमिसेज को परिचय दिया गया और हमें `.then()` और `.catch()` दिया गया, जिससे हमें असिंक्रोनस त्रुटियों को और अधिक सुगमतापूर्वक संभालने में मदद मिली।

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Data fetched:', data))
  .catch(error => console.error('Error fetching data:', error.message));
```

कार्यान्वयन के विवरणों के बारे में, जब एक त्रुटि फेंकी जाती है, जावास्क्रिप्ट इंजन उपयोगी गुणों के साथ एक `Error` ऑब्जेक्ट बनाते हैं जैसे `message` और `stack`। आप अधिक जटिल एप्लिकेशनों के लिए `Error` क्लास का विस्तार करके कस्टम त्रुटि प्रकार भी बना सकते हैं – यह उपयोगी हो सकता है।

विकल्प? आप त्रुटि संचालन को अनदेखा कर सकते हैं (खराब विचार), त्रुटि-प्रथम पैरामीटर्स के साथ कॉलबैक का उपयोग कर सकते हैं (नमस्ते, Node.js शैली), या लाइब्रेरीज और फ्रेमवर्क्स के साथ और भी उन्नती कर सकते हैं जो उनके नजरिए से पेशकश करते हैं।

## और देखें

त्रुटि संचालन पर अधिक के लिए:

- MDN पर try-catch: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- प्रॉमिसेज के लिए एक मार्गदर्शिका: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- कस्टम त्रुटियों को बनाना और फेंकना: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
