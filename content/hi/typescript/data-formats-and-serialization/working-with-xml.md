---
title:                "XML के साथ काम करना"
aliases:
- /hi/typescript/working-with-xml/
date:                  2024-01-26T04:37:15.846725-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ काम करना का तात्पर्य प्रोग्रामिंग का उपयोग करके XML डेटा को पार्स करना, हेरफेर करना, और लिखना है। प्रोग्रामर विभिन्न सिस्टमों के बीच डेटा का आदान-प्रदान करने, कॉन्फ़िगरेशन फ़ाइलों के लिए, या SOAP जैसे मानकों के साथ काम करने के लिए, जो XML पर निर्भर करते हैं, के लिए XML को संभालते हैं।

## कैसे करें:
```TypeScript
import { parseString } from 'xml2js';

// नमूना XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>मीटिंग को मत भूलना!</body>
             </note>`;

// XML को JSON में पार्स करें
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// मान लें कि पार्स सफल रहा, तो आउटपुट इस तरह दिख सकता है:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['मीटिंग को मत भूलना!'] } 
}
```

## गहराई से समझें
XML, या Extensible Markup Language, 90 के दशक के अंत से आसपास रहा है। इसकी आत्म-वर्णनात्मक प्रकृति और मानव-पठनीय प्रारूप ने इसे आरंभ में RSS फीड्स, कॉन्फ़िगरेशन प्रबंधन, और यहाँ तक कि Microsoft Office Open XML जैसे ऑफिस दस्तावेज़ प्रारूपों के लिए विभिन्न अनुप्रयोगों के लिए लोकप्रिय बना दिया। लेकिन, यह JSON की तुलना में बहुवाची है, और समय आ गया है। JSON वेब-आधारित APIs के लिए हल्के वज़न और मूल JavaScript संगतता के कारण स्पॉटलाइट में आया है।

फिर भी, XML मरा नहीं है। यह बड़े पैमाने पर एंटरप्राइज़ सिस्टमों में और डॉक्यूमेंट मानकों के लिए उपयोग किया जाता है जो JSON में नहीं शिफ्ट हुए हैं। `xml2js` जैसे TypeScript के लिए और Python में `lxml` जैसे उपकरण इस बात का प्रमाण हैं कि प्रोग्रामिंग में XML मैनिपुलेशन के लिए लगातार आवश्यकता है।

TypeScript में JSON के लिए जो बिल्ट-इन सपोर्ट है, वैसा XML के लिए नहीं है। इसके बजाय, आप लाइब्रेरीज के साथ काम करते हैं। `xml2js` एक उदाहरण है। यह XML को JSON में बदल देता है, जिससे डेटा को JavaScript गुरुओं के लिए काम करना आसान हो जाता है।

## और देखें
- [MDN Web Docs पर XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm पैकेज](https://www.npmjs.com/package/xml2js)
- [W3Schools XML ट्यूटोरियल](https://www.w3schools.com/xml/)
