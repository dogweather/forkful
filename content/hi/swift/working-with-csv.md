---
title:                "Swift: CSV के साथ काम करना।"
simple_title:         "CSV के साथ काम करना।"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप Swift प्रोग्रामिंग में काम करने में दिक्कत होती है? आपको डेटा को एक स्प्रेडशीट में संग्रहीत करने और इसे एकत्र करने की जरूरत होती है? तो CSV (Comma Separated Values) फॉर्मेट आपकी मदद कर सकता है। CSV फॉर्मेट डेटा को एक लंबित सूची की तरह प्रदर्शित करता है जो प्रोग्रामिंग में उपयोगी हो सकती है। इस ब्लॉग पोस्ट के माध्यम से हम आपको बताएंगे कि CSV क्या है और Swift में CSV को कैसे प्रोसेस किया जा सकता है।

## कैसे करें

CSV फॉर्मेट को Swift में प्रोसेस करना बहुत आसान है। पहले, हमें CSV फाइल का पढ़ने और लिखने के लिए विशेष लाइब्रेरी CSV.swift को इम्पोर्ट करना होगा। फिर, हमें CSV फाइल को अपने कोड के संरचनात्मक रूप में फॉर्मेट करने के लिए नीली संरचनात्मक नोटेशन (NSFormatter) का उपयोग करना होगा। नीचे दिए गए कोड ब्लॉक में, हम एक CSV फाइल से डेटा पढ़ेंगे और उसे प्रदर्शित करेंगे:

 ```Swift
 let csvFile = "Name,Age,City \nJohn,25,New York \nSara,28,Paris \nMike,32,Tokyo"
 
 let csv = try! CSVReader(string: csvFile)
 
 while let row = csv.next() {
     guard let name = row[0], let age = row[1], let city = row[2] else { continue }
     print("\(name) is \(age) years old and lives in \(city).")
 }
 
 // Output:
 // John is 25 years old and lives in New York.
 // Sara is 28 years old and lives in Paris.
 // Mike is 32 years old and lives in Tokyo.
 ```
 
 ऊपर दिए गए कोड ब्लॉक में, हमने सरल CSV फाइल एक स्ट्रिंग के रूप में लिखी है, लेकिन आप किसी भी CSV फाइल को यहां उपयोग कर सकते हैं। दूसरे पैरामीटर में, हमने डाउनलोड किए गए CSV फाइल के ल