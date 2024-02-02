---
title:                "पाठ की खोज एवं प्रतिस्थापन"
date:                  2024-02-01T22:02:34.039827-07:00
model:                 gpt-4-0125-preview
simple_title:         "पाठ की खोज एवं प्रतिस्थापन"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Google Apps Script में पाठ को खोजना और बदलना एक दस्तावेज़, स्प्रेडशीट, या Google Apps की किसी भी अन्य प्रकार की सामग्री में विशिष्ट स्ट्रिंग्स की पहचान करके और उन्हें अन्य पाठ मूल्यों के साथ बदलने की प्रक्रिया को शामिल करता है। प्रोग्रामर बड़ी मात्रा में सामग्री को स्वचालित रूप से संपादित करने, सामान्य त्रुटियों को सही करने, दस्तावेज़ों के भर में शब्दावली को मानकीकृत करने, या टेम्पलेट्स में गतिशील डेटा डालने के लिए इस कार्यक्षमता का उपयोग करते हैं।

## कैसे:

Google Apps Script, विशेष रूप से Google Docs और Sheets के भीतर, पाठ को खोजने और बदलने का एक सीधा तरीका प्रदान करता है। नीचे दोनों के उदाहरण हैं।

### Google Docs:

Google Document में पाठ को खोजने और बदलने के लिए, आप मुख्य रूप से `DocumentApp` क्लास के साथ इंटरैक्ट करेंगे।

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // एक विशिष्ट वाक्यांश को खोजने और बदलने के लिए
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// उपयोग
searchReplaceInDoc();
```

यह कोड स्निपेट सक्रिय Google Document में `'searchText'` के सभी उदाहरणों को खोजता है और उन्हें `'replacementText'` से बदल देता है।

### Google Sheets:

इसी तरह, Google Sheets में, आप खोज और बदलने की क्रियाओं को प्रदर्शित करने के लिए `SpreadsheetApp` का उपयोग कर सकते हैं:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // वर्तमान में सक्रिय शीट में खोज और बदलने के लिए
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// उपयोग
searchReplaceInSheet();
```

इस उदाहरण में, `createTextFinder('searchText')` सक्रिय शीट में 'searchText' के लिए खोजता है, और `replaceAllWith('replacementText')` सभी उदाहरणों को 'replacementText' से बदल देता है।

## गहराई से विचार

Google Apps Script में खोज और बदलने की कार्यक्षमता इसकी वेब-आधारित प्रकृति से गहराई से प्रभावित होती है, जिससे स्क्रिप्ट Google Apps के विभिन्न रूपों में टेक्स्ट को सहजता से संचालित कर सकती है। ऐतिहासिक रूप से, यह क्षमता प्रोग्रामिंग में टेक्स्ट प्रोसेसिंग और मैनिपुलेशन के व्यापक संदर्भ से आती है, जहाँ Perl और Python जैसी भाषाओं में नियमित अभिव्यक्तियाँ और स्ट्रिंग फ़ंक्शन्स लचीलेपन और शक्ति के लिए एक उच्च मानक स्थापित करती हैं।

Google Apps Script की खोज और बदलने की कार्यक्षमता सरल बदलावों के लिए शक्तिशाली है, लेकिन कुछ अन्य भाषाओं में पाए जाने वाले पूर्ण नियमित अभिव्यक्ति क्षमताओं की कमी है। उदाहरण के लिए, जबकि आप Google Sheets में `createTextFinder` में बुनियादी नियमित अभिव्यक्तियों का उपयोग कर सकते हैं, जटिल पैटर्न मिलान और मैनिपुलेशन के विकल्प Perl या Python की तुलना में सीमित हैं।

अधिक उन्नत टेक्स्ट-प्रोसेसिंग आवश्यकताओं के लिए, प्रोग्रामर शायद Google Docs या Sheets सामग्री को एक ऐसे स्वरूप में निर्यात करने के लिए पसंद कर सकते हैं जो बाहरी रूप से अधिक शक्तिशाली भाषाओं के साथ संसाधित किया जा सके या अधिक सोफिस्टिकेटेड टेक्स्ट मैनिपुलेशन क्षमताओं की पेशकश करने वाली बाहरी APIs या सेवाओं को कॉल करने के लिए Google Apps Script का उपयोग कर सकते हैं।

इन सीमाओं के बावजूद, Google Apps के पारिस्थितिकी तंत्र के भीतर अधिकांश विशिष्ट खोज और बदलने के कार्यों के लिए, Google Apps Script एक सरल, कुशल, और उच्चतम एकीकरण योग्य समाधान प्रदान करता है जो Google के उत्पादकता उपकरणों के सूट के भीतर स्वचालन और स्क्रिप्टिंग की आवश्यकताओं के लिए उपयुक्त है।