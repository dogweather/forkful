---
title:                "डायरेक्टरी का अस्तित्व जांचना"
html_title:           "TypeScript: डायरेक्टरी का अस्तित्व जांचना"
simple_title:         "डायरेक्टरी का अस्तित्व जांचना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डायरेक्टरी की जांच करना क्या है और क्यों प्रोग्रामर इसे करते हैं? डायरेक्टरी जांच करना एक आम टास्क है जो प्रोग्रामरों को उनके कोड में एक निश्चित फ़ोल्डर की उपस्थिति की जांचने की अनुमति देता है। जब आप किसी निश्चित फ़ोल्डर के उपस्थिति को सुनिश्चित करते हैं, तो आप अनुचित एक्शन से बच सकते हैं और अपने कोड को सुरक्षित रख सकते हैं।

## कैसे करें:

```TypeScript
if (fs.existsSync('directory/path')) {
  console.log('फ़ोल्डर मौजूद है।');
} else {
  console.log('फ़ोल्डर नहीं मौजूद है।');
}
```

```
फ़ोल्डर मौजूद है।
```

```TypeScript
// async विधि से डायरेक्टरी की जांच करें
fs.access('directory/path', (err) => {
  if (err) {
    console.log('फ़ोल्डर नहीं मौजूद है।');
  } else {
    console.log('फ़ोल्डर मौजूद है।');
  }
});
```

```
फ़ोल्डर मौजूद है।
```

## गहराई में:

(1) ऐतिहासिक संदर्भ - डायरेक्टरी की जांच से पहले, प्रोग्रामरों को उपनाम और फ़ोल्डर की जांच करनी पड़ती थी। (2) वैकल्पिक - डायरेक्टरी की जांच करने के दूसरे तरीके, जैसे उपनाम या यूआरएल, मौजूद है। लेकिन यह ऋणात्मक परिणाम को उत्पन्न कर सकता है जो मूल कोड को कंभल्ट करने से रोक सकता है। (3) परिचालन विवरण - ```fs.existsSync()``` और ```fs.access()``` विधियों का उपयोग कैसे किया जाता है डायरेक्टरी की जांच करने के लिए और कैसे उपलब्ध समानताएं हैं।

## देखें भी:

- [Node.js Documentation on fs module](https://nodejs.org/api/fs.html)
- [Tutorial on Node.js File System](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Article on Folder/File Exist in JavaScript](https://www.geeksforgeeks.org/how-to-check-whether-a-directory-exists-using-javascript/)