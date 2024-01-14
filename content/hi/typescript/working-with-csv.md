---
title:                "TypeScript: csv से काम करना"
simple_title:         "csv से काम करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

CSV (Comma Separated Values) फ़ाइलें डेटा को आसानी से संपूर्ण करने और उपयोग करने के लिए उपयोगी होती हैं। TypeScript में CSV फ़ाइलों के साथ काम करने से, आप अपने डेटा को जांच, संपादित और विश्लेषण कर सकते हैं। यह आपको अपने डेटा पर अधिक नियंत्रण और सुविधा देता है। 

## कैसे करें

आप निम्नलिखित स्टेप्स का पालन करके CSV फ़ाइलों को TypeScript में कैसे पढ़ा जाए, सीख सकते हैं: 

1. ```TypeScript``` को अपने सिस्टम पर इंस्टॉल करें।
2. CSV पैकेज को इंस्टॉल करें: ```npm install csv```
3. CSV फ़ाइल को TypeScript एप्लिकेशन में इम्पोर्ट करें: ```import * as csv from 'csv';```
4. CSV फ़ाइल में स्तंभों के नाम और डेटा धाराओं का अनुक्रम निर्दिष्ट करें। 
5. CSV फ़ाइल से डेटा पढ़े: ```csv.parse(data, {columns: true}, (err, data) => { // code to handle data })```
6. डेटा को CSV फ़ाइल में लिखें: ```csv.stringify(data, {header: true}, (err, data) => { // code to handle data })```

यहां आपको आपके CSV फ़ाइल को पढ़ने और लिखने के लिए स्ट्रीम्स का उपयोग करना पड़ सकता है। आप सीधे स्ट्रीम ऑब्जेक्ट्स का भी प्रयोग कर सकते हैं। 

## गहराई में जाएं 

CSV फ़ाइलें डेटा में सामान्यता से बहुत अधिक उपयोग होती हैं और आपको अपने डेटा पर पूर्ण नियंत्रण और सुविधा देती हैं। आप अपने एप्लिकेशन में दूसरे लाइब्रेरी जैसे ```json2csv``` का भी प्रयोग कर सकते हैं जो अधिक गहराई में CSV फ़ाइल उत्पन्न करने की सुविधा द