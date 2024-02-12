---
title:                "तारीख को स्ट्रिंग में परिवर्तित करना"
date:                  2024-02-01T21:52:41.686919-07:00
model:                 gpt-4-0125-preview
simple_title:         "तारीख को स्ट्रिंग में परिवर्तित करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/converting-a-date-into-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीखों को स्ट्रिंग्स में बदलना एक मौलिक कार्य है जो प्रोग्रामरों को तारीख की जानकारी को मनुष्य के पढ़ सकने वाले प्रारूप में संभालने और प्रदर्शित करने की अनुमति देता है। यह उपयोगकर्ता इंटरफ़ेस बनाने, रिपोर्ट जेनरेट करने, या Google Apps Script के साथ विकसित किए गए अनुप्रयोगों में जानकारी लॉग करने के लिए महत्वपूर्ण है।

## कैसे:

Google Apps Script, JavaScript पर आधारित होने के कारण, तारीखों को स्ट्रिंग में बदलने के लिए कई विधियाँ प्रदान करता है। नीचे कुछ उदाहरण विभिन्न दृष्टिकोणों का वर्णन कर रहे हैं:

### `toString()` विधि का उपयोग करना:
सबसे सरल विधि `toString()` विधि का उपयोग करना है, जो तारीख ऑब्जेक्ट को डिफ़ॉल्ट प्रारूप में एक स्ट्रिंग में बदल देता है।

```javascript
var date = new Date();  // एक नई तारीख ऑब्जेक्ट बनाता है
var dateString = date.toString();
Logger.log(dateString); // आउटपुट: "Wed Apr 05 2023 12:34:56 GMT-0700 (Pacific Daylight Time)"
```

### `toDateString()` विधि का उपयोग करना:
समय की जानकारी के बिना केवल तारीख भाग को पढ़ने योग्य प्रारूप में प्राप्त करने के लिए `toDateString()` का प्रयोग किया जा सकता है।

```javascript
var date = new Date();
var dateString = date.toDateString();
Logger.log(dateString); // आउटपुट: "Wed Apr 05 2023"
```

### कस्टम प्रारूपों के लिए `Utilities.formatDate()` का उपयोग करना:
प्रारूप पर अधिक नियंत्रण के लिए, Google Apps Script `Utilities.formatDate()` प्रदान करता है। इस विधि में तीन पैरामीटर आवश्यक होते हैं: तारीख ऑब्जेक्ट, समय क्षेत्र, और प्रारूप स्ट्रिंग।

```javascript
var date = new Date();
var timeZone = Session.getScriptTimeZone();
var formattedDate = Utilities.formatDate(date, timeZone, "YYYY-MM-dd");
Logger.log(formattedDate); // आउटपुट: "2023-04-05"
```

यह विधि विशेष रूप से उन प्रारूपों में तारीखें जेनरेट करने में शक्तिशाली है जो स्थानीय-विशेष या विशिष्ट अनुप्रयोग आवश्यकताओं के अनुकूल होते हैं।

## गहरा डाइव

तारीखों को स्ट्रिंग्स में बदलने की आवश्यकता केवल Google Apps Script तक सीमित नहीं है; यह सभी प्रोग्रामिंग भाषाओं में प्रचलित है। हालांकि, JavaScript से विरासत में मिली Google Apps Script की दृष्टिकोण वेब-आधारित स्क्रिप्टिंग के प्रति लचीला विकल्पों का एक समूह प्रदान करती है। समय क्षेत्र के साथ काम करने की जटिलताओं को स्वीकार करते हुए `Utilities.formatDate()` खड़ा होता है - एक चुनौती जिसे अक्सर अनदेखा किया जाता है। 

ऐतिहासिक रूप से, तारीखों और समयों को संभालना सॉफ्टवेयर विकास में बग्स और जटिलता का एक स्रोत रहा है, मुख्यतः समय क्षेत्रों और प्रारूपों में अंतर के कारण। Google Apps Script में `Utilities.formatDate()` का परिचय दुनिया भर में इस्तेमाल किए जाने वाले Google के उत्पादों के संदर्भ में विशेष रूप से तारीख-समय मैनिपुलेशन को मानकीकृत करने की ओर एक कदम है।

हालांकि, जब समय क्षेत्रों, स्थानीय इकाइयों, और प्रारूपों पर सटीक नियंत्रण की आवश्यकता होती है, विशेष रूप से अंतरराष्ट्रीयकृत अनुप्रयोगों में, विकासकर्ता `Moment.js` जैसे बाहरी पुस्तकालयों का उपयोग कर सकते हैं (बंडल आकार की चिंताओं और आधुनिक विशेषताओं के कारण `Luxon`, `Day.js`, और `date-fns` के लिए बढ़ती प्राथमिकता के बावजूद)। यह दृष्टिकोण, निश्चित रूप से, बाहरी निर्भरताओं को जोड़ने और संभावित रूप से परियोजना की जटिलता में वृद्धि के साथ आता है। 

बाहरी पुस्तकालयों की संभावितता के बावजूद, `Utilities.formatDate()` और नेटिव JavaScript तारीख विधियाँ अधिकांश सामान्य उपयोगिता मामलों के लिए मजबूत समाधान प्रदान करती हैं। समझदार विकासकर्ता अपनी परियोजना की विशिष्ट आवश्यकताओं के आधार पर निर्मित-इन फ़ंक्शनों की सादगी और सुविधा के साथ बाहरी पुस्तकालयों की शक्ति और लचीलापन को संतुलित करेंगे।