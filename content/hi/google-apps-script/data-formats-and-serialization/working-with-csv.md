---
title:                "CSV के साथ काम करना"
aliases:
- /hi/google-apps-script/working-with-csv/
date:                  2024-02-01T22:06:53.805278-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/working-with-csv.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Google Apps Script में CSV (Comma-Separated Values) फाइलों के साथ काम करना ऐसी प्लेन-टेक्स्ट फाइलों को पढ़ने, संशोधित करने, और लिखने का प्रक्रिया है जहाँ प्रत्येक पंक्ति एक डेटा रिकॉर्ड को दर्शाती है जिसके मान अल्पविरामों द्वारा अलग किए गए होते हैं। प्रोग्रामर विभिन्न एप्लिकेशन्स, डेटाबेस, या प्रोग्रामिंग भाषाओं के बीच डेटा का आसानी से आदान-प्रदान करने के लिए यह काम करते हैं क्योंकि CSV को एक सरल, पाठ-आधारित डेटा आदान-प्रदान प्रारूप के रूप में व्यापक रूप से अपनाया गया है।

## कैसे:

### CSV डेटा पढ़ना

Google Drive में संग्रहीत एक फाइल से CSV डेटा पढ़ने के लिए, आपको पहले फाइल की सामग्री को एक स्ट्रिंग के रूप में प्राप्त करने की आवश्यकता है, फिर उसे पार्स करें। Google Apps Script के साथ फाइल सामग्री प्राप्त करना DriveApp सेवा के साथ सरलीकृत है।

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // वास्तविक फाइल ID से बदलें
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // प्रत्येक पंक्ति के कोशिकाओं को लॉग करें
  }
}
```

### CSV डेटा लिखना

CSV बनाने और उसमें लिखने के लिए आल्पविराम-विभाजित मानों और नई पंक्तियों के साथ एक स्ट्रिंग का निर्माण करना और फिर उसे सहेजना या निर्यात करना शामिल है। यह उदाहरण Google Drive में एक नई CSV फाइल बनाने का तरीका दर्शाता है।

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // नई फाइल बनाई जाने वाली Drive फ़ोल्डर की ID से बदलें
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### नमूना आउटपुट

CSV से पंक्ति कोशिकाओं को लॉग करते समय:

```plaintext
[John, 29, Engineer]
[Jane, 34, Designer]
```

लिखते समय, एक "example.csv" नामक फाइल "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer" सामग्री के साथ बनाई गई है:

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## गहराई से

ऐतिहासिक रूप से, CSV फाइलों को उनकी सरलता और मानव-पठनीयता के लिए पसंद किया गया है, जिससे वे गैर-प्रोग्रामरों के लिए सुलभ होते हैं और त्वरित डेटा-निरीक्षण कार्यों के लिए उपयोगी होते हैं। हालांकि, Google Apps Script Google के इकोसिस्टम के भीतर काम करता है, जहाँ Google Sheets CSV हेरफेर के लिए एक शक्तिशाली, उपयोगकर्ता-अनुकूल विकल्प के रूप में कार्य करता है। Sheets न केवल डेटा संपादन के लिए एक GUI प्रदान करते हैं, बल्कि जटिल सूत्रों, स्टाइलिंग, और कई अन्य सुविधाओं का समर्थन भी करते हैं जो कच्चे CSV फाइलों में नहीं होते।

फिर भी, Google Sheets द्वारा प्रदान की गई लाभों के बावजूद, Google Apps Script में सीधे CSV हेरफेर बाहरी सिस्टमों के साथ स्वचालित कार्यों के लिए महत्वपूर्ण बना हुआ है, विशेष रूप से जब बाहरी सिस्टमों द्वारा उत्पन्न या CSV प्रारूप में डेटा की आवश्यकता होती है। उदाहरण के लिए, पुराने सिस्टमों के साथ एकीकरण, अन्य अनुप्रयोगों में उपयोग के लिए डेटा निर्यात करना, या Google Sheets में डेटा डालने से पहले प्रीप्रोसेसिंग।

इसके अतिरिक्त, Google Apps Script की CSV फाइलों के साथ काम करने की क्षमता उन्नत एन्कोडिंग आवश्यकताओं के लिए Utilities सेवा के साथ बढ़ाई जा सकती है, या परिवर्तन, पार्सिंग, या सत्यापन कार्यों के लिए बाह्य APIs के साथ इंटरफेस की जा सकती है। हालांकि, बड़े डेटासेटों के साथ काम करने या जटिल हेरफेर की आवश्यकता वाले लिए, अधिक मजबूत डेटा प्रोसेसिंग क्षमताओं के लिए Google Sheets APIs का लाभ उठाने या BigQuery का पता लगाने पर विचार करें।

CSV की लोकप्रियता के लिए सादगी एक प्रमुख कारण बनी हुई है, फिर भी ये विकल्प विस्तृत Google Cloud इकोसिस्टम में डेटा से निपटने के लिए सुविधाओं का अधिक समृद्ध सेट प्रदान करते हैं।
