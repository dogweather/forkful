---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:24.463723-07:00
description: "Google Apps Script \u092E\u0947\u0902 \u090F\u0915 \u0921\u093E\u092F\
  \u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u0915\u0947 \u0905\u0938\u094D\u0924\
  \u093F\u0924\u094D\u0935 \u0915\u0940 \u091C\u093E\u0901\u091A \u0915\u0930\u0928\
  \u093E, Google Drive \u092E\u0947\u0902 \u090F\u0915 \u092B\u094B\u0932\u094D\u0921\
  \u0930 \u0915\u0940 \u0909\u092A\u0938\u094D\u0925\u093F\u0924\u093F \u0915\u0940\
  \ \u0938\u0924\u094D\u092F\u093E\u092A\u0928\u093E \u0915\u094B \u0926\u0930\u094D\
  \u0936\u093E\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930 \u0905\u0915\u094D\u0938\u0930 \u092F\u0939 \u091C\u093E\
  \u0901\u091A\u2026"
lastmod: '2024-03-13T22:44:51.540374-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u092E\u0947\u0902 \u090F\u0915 \u0921\u093E\u092F\u0930\
  \u0947\u0915\u094D\u091F\u0930\u0940 \u0915\u0947 \u0905\u0938\u094D\u0924\u093F\
  \u0924\u094D\u0935 \u0915\u0940 \u091C\u093E\u0901\u091A \u0915\u0930\u0928\u093E\
  , Google Drive \u092E\u0947\u0902 \u090F\u0915 \u092B\u094B\u0932\u094D\u0921\u0930\
  \ \u0915\u0940 \u0909\u092A\u0938\u094D\u0925\u093F\u0924\u093F \u0915\u0940 \u0938\
  \u0924\u094D\u092F\u093E\u092A\u0928\u093E \u0915\u094B \u0926\u0930\u094D\u0936\
  \u093E\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930 \u0905\u0915\u094D\u0938\u0930 \u092F\u0939 \u091C\u093E\u0901\
  \u091A\u2026"
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902 \u091C\u093E\u0902\
  \u091A\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

Google Apps Script में एक डायरेक्टरी के अस्तित्व की जाँच करना, Google Drive में एक फोल्डर की उपस्थिति की सत्यापना को दर्शाता है। प्रोग्रामर अक्सर यह जाँच करते हैं ताकि फाइलों और डायरेक्टरियों को कार्यक्रमात्मक रूप से प्रबंधित करते समय त्रुटियों या अनावश्यक फोल्डर सृजन से बचा जा सके।

## कैसे:

Google Apps Script सीधे तौर पर फोल्डरों के लिए कोई "exists" विधि प्रदान नहीं करता है। इसके बजाय, हम Google Drive की खोज क्षमताओं का उपयोग करके यह जाँचते हैं कि किसी विशिष्ट नाम वाला एक फोल्डर मौजूद है या नहीं। यहाँ एक कदम-दर-कदम उदाहरण दिया गया है:

```javascript
// यह जाँचने के लिए फ़ंक्शन कि कोई डायरेक्टरी मौजूद है या नहीं
function checkIfDirectoryExists(directoryName) {
  // उस निर्दिष्ट नाम के अनुरूप फोल्डरों का संग्रह प्राप्त करें
  var folders = DriveApp.getFoldersByName(directoryName);
  
  // जाँच करें कि क्या निर्दिष्ट नाम वाला कम से कम एक फोल्डर मौजूद है
  if (folders.hasNext()) {
    Logger.log('डायरेक्टरी मौजूद है.');
    return true;
  } else {
    Logger.log('डायरेक्टरी मौजूद नहीं है.');
    return false;
  }
}

// उदाहरण के लिए
var directoryName = 'My Sample Folder';
checkIfDirectoryExists(directoryName);
```

नमूना आउटपुट:
```
डायरेक्टरी मौजूद है.
```
या 
```
डायरेक्टरी मौजूद नहीं है.
```

यह स्क्रिप्ट `getFoldersByName` विधि का उपयोग करती है जो निर्दिष्ट नाम से मेल खाने वाले उपयोगकर्ता के ड्राइव में सभी फोल्डरों को पुनः प्राप्त करती है। चूंकि Drive में नाम अद्वितीय नहीं होते हैं, यह विधि एक `FolderIterator` लौटाती है। इस इटेरेटर में एक अगली आइटम (`hasNext()`) की उपस्थिति यह दर्शाती है कि निर्देशिका मौजूद है।

## गहराई से जानकारी

इतिहास में, वेब और क्लाउड वातावरण में फ़ाइल प्रबंधन में महत्वपूर्ण विकास हुआ है। Google Apps Script, Google Drive के लिए एक व्यापक API प्रदान करते हुए, खोज और जाँच तंत्रों सहित सोफ़िस्टिकेटेड फ़ाइल और फोल्डर प्रबंधन ऑपरेशंस के लिए अनुमति देता है। हालाँकि, एक नोटेबल पहलू यह है कि सीधे अस्तित्व की जांच का अभाव है, शायद इसलिए क्योंकि Google Drive, एक ही नाम के कई फोल्डरों की अनुमति देता है, जो कई फ़ाइल सिस्टमों के विपरीत है जो एक ही निर्देशिका में अद्वितीय नामों को लागू करते हैं।

इस संदर्भ में, `getFoldersByName` विधि का उपयोग एक प्रभावी वर्कअराउंड है लेकिन जहां विशाल संख्या में डुप्लिकेट नामों वाले फोल्डर मौजूद हों, वहां इससे अक्षमता पैदा हो सकती है। एक वैकल्पिक दृष्टिकोण एप्लिकेशन-विशिष्ट अनुक्रमण या नामकरण सम्मेलन को बनाए रखने में शामिल हो सकता है ताकि विशेष रूप से जब प्रदर्शन एक महत्वपूर्ण चिंता बन जाए, तो तेजी से जाँच सुनिश्चित की जा सके।

जबकि Google Apps Script का दृष्टिकोण एकल फ़ाइल सिस्टम के साथ सीधे इंटरफ़ेस की गई प्रोग्रामिंग भाषाओं में फ़ाइल अस्तित्व जाँच की तुलना में प्रारंभिक रूप से कम सीधा प्रतीत हो सकता है, यह क्लाउड-आधारित फ़ाइल स्टोरेज की जटिलताओं को संभालने की आवश्यकता को दर्शाता है। Google Apps Script का उपयोग करने वाले डेवलपर्स को Google Drive की ताकतों और सीमाओं के लिए इन बारीकियों पर विचार करना चाहिए, इसके लिए अनुकूलन करते हुए।
