---
title:                "HTML पार्स करना"
aliases:
- /hi/google-apps-script/parsing-html/
date:                  2024-02-01T21:58:49.937709-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML पार्स करना"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/parsing-html.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Google Apps Script में HTML पार्सिंग का अर्थ है HTML सामग्री से डेटा निकालना, जो वेब पृष्ठों या वेब-आधारित डेटा स्रोतों के साथ बातचीत करते समय विशेष रूप से उपयोगी होता है। प्रोग्रामर इसे डेटा संग्रहण को स्वचालित करने, वेब सामग्री को संशोधित करने, या Google Apps जैसे Sheets और Docs के साथ वेब कार्यक्षमताओं को एकीकृत करने के लिए करते हैं।

## कैसे:
Google Apps Script में HTML को पार्स करने की कोई निर्मित विधि नहीं है। हालांकि, आप HTML सामग्री प्राप्त करने के लिए `UrlFetchApp` सेवा का उपयोग कर सकते हैं और फिर पार्सिंग के लिए JavaScript विधियों या regex (नियमित अभिव्यक्तियों) का उपयोग कर सकते हैं। नीचे वेबपेज से टाइटल टैग फेच करने और पार्स करने का एक मूल उदाहरण दिया गया है।

```javascript
function parseHTMLTitle(url) {
  // वेबपेज की HTML सामग्री फेच करें
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // <title> टैग की सामग्री खोजने के लिए एक सरल regex का उपयोग करें
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // जांचें कि क्या एक शीर्षक पाया गया था और उसे वापस करें
  if (match && match.length > 1) {
    return match[1];
  }

  return 'कोई शीर्षक नहीं मिला';
}

// उदाहरण उपयोग
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // वेबपेज का शीर्षक आउटपुट करता है
```

अधिक उन्नत HTML पार्सिंग के लिए, आप HTML को XML के रूप में पार्स करने के लिए `XmlService` का उपयोग कर सकते हैं। हालांकि, इसके लिए आवश्यक है कि HTML अच्छी तरह से बनी XML हो, जो हमेशा मामला नहीं होता:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // यहां से, XmlService विधियों के साथ XML ट्री के माध्यम से नेविगेट करें
    // उदाहरण के लिए, एक विशिष्ट तत्व या विशेषता खोजने के लिए
  } catch(e) {
    Logger.log('HTML पार्सिंग में त्रुटि: ' + e.toString());
  }
}
```

## गहराई से:
ऐतिहासिक रूप से, Google Apps Script जैसे वातावरणों में HTML पार्सिंग चुनौतीपूर्ण रही है क्योंकि वहां एक समर्पित पार्सिंग लाइब्रेरीज या डॉक्यूमेंट ऑब्जेक्ट मॉडल (DOM) की कमी होती है, जो अन्य प्रोग्रामिंग संदर्भों में आम होती हैं। उदाहरण के लिए, ब्राउजर में JavaScript के पास DOM सहजता से उपलब्ध होता है, और Node.js वातावरणों को `cheerio` या `jsdom` जैसे NPM पैकेजों तक पहुँच होती है जो HTML को पार्स करने के लिए होते हैं।

Google Apps Script का दृष्टिकोण `UrlFetchApp` का उपयोग करके वेब अनुरोधों के लिए और फिर regex या XML पार्सिंग विधियों का उपयोग करके प्रतिक्रिया डेटा को संशोधित करने पर अधिक झुकाव रखता है। जबकि regex सरल पार्सिंग कार्यों के लिए उपयोगी हो सकता है, यह जटिल HTML के लिए सामान्यतः सलाह नहीं दी जाती क्योंकि त्रुटियों का खतरा होता है और कोड का संभावित नाजुक स्वभाव हो सकता है। XML पार्सिंग `XmlService` के साथ एक अधिक संरचित दृष्टिकोण प्रदान करती है लेकिन अच्छी तरह से बने HTML/XML की आवश्यकता होती है, जो मनमाने वेब पृष्ठों से निपटने पर एक सीमा हो सकती है।

जटिल पार्सिंग आवश्यकताओं या खराब बने HTML से निपटते समय, एक वैकल्पिक रणनीति Google Apps Script के बाहर एक वेब सेवा का उपयोग करना शामिल हो सकती है। यह सेवा HTML सामग्री को संसाधित कर सकती है, संभवतः एक अधिक मजबूत पार्सिंग तकनीक या लाइब्रेरी का उपयोग कर, और फिर प्रोसेस किए गए डेटा को Google Apps Script द्वारा आसानी से उपयोग करने योग्य फॉर्म में वापस कर सकती है। हालांकि, यह दृष्टिकोण नेटवर्क विलंबता और एक अतिरिक्त वेब सेवा का प्रबंधन करने की जटिलता को पेश करता है।

इन चुनौतियों के बावजूद, Google Apps Script के भीतर HTML पार्सिंग, विशेष रूप से अन्य Google सेवाओं और APIs के साथ संयोजन में, एक शक्तिशाली उपकरण बनी हुई है, जो उत्पादकता और डेटा प्रोसेसिंग क्षमताओं को काफी बढ़ा सकती है।
