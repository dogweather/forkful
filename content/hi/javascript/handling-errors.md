---
title:                "एरर्स को हैंडल करना"
aliases:
- hi/javascript/handling-errors.md
date:                  2024-01-26T00:55:18.732187-07:00
model:                 gpt-4-1106-preview
simple_title:         "एरर्स को हैंडल करना"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/handling-errors.md"
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
