---
title:                "नया प्रोजेक्ट शुरू करना"
date:                  2024-02-01T22:05:03.596980-07:00
model:                 gpt-4-0125-preview
simple_title:         "नया प्रोजेक्ट शुरू करना"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/starting-a-new-project.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Google Apps Script (GAS) में एक नई परियोजना शुरू करना का मतलब है Google पारिस्थितिकी तंत्र (Google Drive, Docs, Sheets, आदि) में एक स्क्रिप्ट फ़ाइल को आरंभ करना जिसे कार्यों को स्वचालित करने या Google Apps की कार्यक्षमताओं का विस्तार करने के लिए इस्तेमाल किया जा सकता है। प्रोग्रामर्स अक्सर इस यात्रा पर आधारित होते हैं ताकि वे कार्यप्रवाह को सरलीकृत कर सकें, Google सेवाओं को प्रोग्रामेटिक रूप से संभाल सकें, या कस्टम एड-ओन्स बना सकें, समय बचा सकें और Google के अवसंरचना की शक्ति का उपयोग कर सकें।

## कैसे करें:

Google Apps Script में एक नई परियोजना शुरू करने के लिए, आपके पास कुछ प्रवेश बिंदु होते हैं, लेकिन चलिए सबसे सीधे तरीके पर ध्यान केंद्रित करते हैं: Google Drive से स्क्रिप्ट बनाना।

1. **Google Drive में एक परियोजना बनाना**
   - Google Drive (drive.google.com) पर जाएं।
   - "+ न्यू" > "और" > "Google Apps Script" पर क्लिक करें।
   - एक नया स्क्रिप्ट परियोजना संपादक में खुलती है। डिफॉल्ट रूप से, इसमें एक `Code.gs` फ़ाइल होती है जिसमें एक नमूना `myFunction` होता है।

2. **अपनी परियोजना सेट अप करना**
   - स्पष्टता के लिए अपनी परियोजना का नाम बदलें। ऊपर बाईं ओर "Untitled project" पर क्लिक करें, और इसे एक अर्थपूर्ण नाम दें।
   - `Code.gs` फ़ाइल में एक सरल फंक्शन लिखें ताकि आपको इसका एक अहसास हो:

```javascript
function helloWorld() {
  Logger.log('Hello, world!');
}
```

   - `helloWorld` को चलाएँ खेल बटन (▶) के बगल में ड्रॉपडाउन में फंक्शन का चयन करके और इसे क्लिक करके। यह फंक्शन को निष्पादित करेगा।

3. **लॉग देखना**
   - `Logger.log` का आउटपुट देखने के लिए, "View" > "Logs" पर जाएं, या `Ctrl + Enter` दबाएँ। आपको लॉग्स में "Hello, world!" दिखाई देना चाहिए।

बधाई हो, आपने Google Apps Script में सफलतापूर्वक एक नई परियोजना शुरू की है और एक सरल फंक्शन चलाया है!

## गहराई में जाना

Google Apps Script का आरंभ 2009 के आसपास हुआ, जिसने डेवलपर्स और गैर-डेवलपर्स दोनों के लिए Google सेवाओं के विस्तृत सरणी को स्वचालित करने, विस्तार करने और उस पर निर्माण करने के लिए एक शक्तिशाली फिर भी सुलभ मंच प्रदान किया। पारंपरिक प्रोग्रामिंग वातावरणों के विपरीत, GAS एक अनूठा मिश्रण प्रदान करता है सादगी और एकीकरण का, सीधे Google पारिस्थितिकी तंत्र में, बाहरी सर्वरों या सेटअप की आवश्यकता के बिना। यह सर्वरलेस निष्पादन मॉडल परियोजना तैनाती और प्रबंधन को बहुत सरल बनाता है।

ऐतिहासिक रूप से, GAS अपने निष्पादन वातावरण और भाषा संस्करण द्वारा कुछ हद तक सीमित था, अक्सर मौजूदा JavaScript मानकों के पीछे रहता था। हालाँकि, हाल के अपडेट्स ने GAS में आधुनिक JavaScript सिंटैक्स (ECMAScript 2015+) ले आए हैं, जिससे यह आधुनिक विकास प्रथाओं के आदी डेवलपर्स के लिए अधिक स्वीकार्य हो गया है।

हालांकि GAS Google Services के साथ बातचीत करने के लिए अनूठी तरह से स्थित है, अधिक तीव्र या विशिष्ट आवश्यकताओं के लिए वैकल्पिक दृष्टिकोण हैं। उदाहरण के लिए, Google Cloud Functions और Google Cloud Platform (GCP) जटिल कार्यप्रवाहों को संभालने, बड़े डेटासेटों को संसाधित करने, और बाहरी API के साथ एकीकरण के लिए अधिक मजबूत और स्केलेबल समाधान प्रदान करते हैं। ये मंच विभिन्न भाषाओं (जैसे कि Python, Go, Node.js) में प्रोग्रामिंग की अनुमति देते हैं और अधिक गणना संसाधन प्रदान करते हैं।

फिर भी, Google Apps के साथ गहराई से जुड़े कार्यों, स्वचालन, और इस पारिस्थितिकी तंत्र के भीतर त्वरित विकास के लिए, Google Apps Script उपयोग में आसानी और एकीकरण की गहराई के मामले में एक अतुलनीय उपकरण बनी हुई है। इसकी सीधी पहुँच Google Drive से और Google सेवाओं के साथ निर्बाध संबंध इसे विविध परियोजनाओं के लिए एक व्यावहारिक विकल्प बनाती है, विशेषतः उनके लिए जो Sheets, Docs, Forms, और अन्य Google अनुप्रयोगों की कार्यक्षमता का विस्तार करना चाहते हैं।