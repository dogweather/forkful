---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:53.344602-07:00
description: "CSV (Comma Separated Values) \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\
  \u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E \u0910\u0938\
  \u0940 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932\u094B\
  \u0902 \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0914\
  \u0930 \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\u0930\u0928\u0947 \u092E\
  \u0947\u0902 \u0936\u093E\u092E\u093F\u0932 \u0939\u0948 \u091C\u0939\u093E\u0901\
  \ \u092A\u094D\u0930\u0924\u094D\u092F\u0947\u0915 \u092A\u0902\u0915\u094D\u0924\
  \u093F \u092E\u0947\u0902 \u092E\u093E\u0928 \u0905\u0932\u094D\u092A\u0935\u093F\
  \u0930\u093E\u092E\u094B\u0902\u2026"
lastmod: '2024-03-13T22:44:51.854421-06:00'
model: gpt-4-0125-preview
summary: "CSV (Comma Separated Values) \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\
  \u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E \u0910\u0938\
  \u0940 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093E\u0907\u0932\u094B\
  \u0902 \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0914\
  \u0930 \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\u0930\u0928\u0947 \u092E\
  \u0947\u0902 \u0936\u093E\u092E\u093F\u0932 \u0939\u0948 \u091C\u0939\u093E\u0901\
  \ \u092A\u094D\u0930\u0924\u094D\u092F\u0947\u0915 \u092A\u0902\u0915\u094D\u0924\
  \u093F \u092E\u0947\u0902 \u092E\u093E\u0928 \u0905\u0932\u094D\u092A\u0935\u093F\
  \u0930\u093E\u092E\u094B\u0902\u2026"
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV (Comma Separated Values) फाइलों के साथ काम करना ऐसी टेक्स्ट फाइलों को पार्स करने और उत्पन्न करने में शामिल है जहाँ प्रत्येक पंक्ति में मान अल्पविरामों द्वारा अलग किए जाते हैं। प्रोग्रामर इसे विभिन्न अनुप्रयोगों के बीच डेटा एक्सचेंज को सक्षम करने या हल्के, मानव-पठनीय प्रारूप में डेटा संग्रहण को सुगम बनाने के लिए करते हैं।

## कैसे:

Dart में CSV फाइलों को संभालने के लिए, आप आम तौर पर टेक्स्ट को मैन्युअल रूप से संसाधित करते हैं या तीसरे पक्ष की लाइब्रेरीज का उपयोग कार्य को सुगम बनाने के लिए करते हैं। यहाँ, हम दोनों दृष्टिकोणों को देखेंगे।

### मैन्युअली पार्सिंग CSV

यदि आपकी आवश्यकताएँ सरल हैं, तो आप एक CSV स्ट्रिंग को मैन्युअली पार्स करना चुन सकते हैं। यह Dart के कोर स्ट्रिंग मैनिपुलेशन फंक्शनों का उपयोग करके हासिल किया जा सकता है:

```dart
void main() {
  // नमूना CSV डेटा
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // CSV डेटा को पंक्तियों में विभाजित करना
  List<String> lines = csvData.split('\n');
  
  // प्रत्येक पंक्ति का पार्सिंग
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // पार्स किए गए डेटा का आउटपुट
  print(data);
}

// नमूना आउटपुट:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### एक तृतीय-पक्ष पुस्तकालय का उपयोग करना: `csv`

अधिक जटिल परिदृश्यों के लिए या अपने कोड को सरल बनाने के लिए, आप जैसे लोकप्रिय तृतीय-पक्ष पुस्तकालय `csv` का उपयोग कर सकते हैं। सबसे पहले, अपनी परियोजना में इसे जोड़ें जिसमें आपकी `pubspec.yaml` फ़ाइल के `dependencies` के अंतर्गत `csv: ^5.0.0` (या नवीनतम संस्करण) शामिल करें। फिर इसे निम्नलिखित तरीके से उपयोग करें:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // CsvToListConverter का उपयोग करके CSV डेटा को पार्स करें
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // पहली सूची आइटम हेडर्स को धारण करती है
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // आगे की प्रक्रिया से पहले हेडर पंक्ति को हटाना
  listData.removeAt(0);
  
  // अधिक संरचित प्रारूप के लिए List<Map<String, dynamic>> में परिवर्तित करें
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // मैप किए गए डेटा का आउटपुट
  print(mappedData);
}

// नमूना आउटपुट:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

दोनों तरीके दिखाते हैं कि CSV डेटा के साथ कैसे काम किया जाता है: पहला मैन्युअली, सीखने के उद्देश्यों के लिए या जब बहुत सरल CSV संरचनाओं से निपटना हो; दूसरा, एक शक्तिशाली पुस्तकालय का लाभ उठाकर जो पार्सिंग को सरल बनाता है और CSV प्रारूपण की विविध जटिलताओं को संभाल सकता है।
