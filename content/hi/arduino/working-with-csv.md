---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
simple_title:         "CSV के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV, यानी 'Comma-separated Values', टेक्स्ट फाइल फॉर्मेट है जिसमें डेटा को अल्पविराम से अलग किया जाता है। Arduino प्रोग्रामर्स इसे डेटा लॉग, सेंसर रीडिंग्स, या कॉन्फ़िगरेशन फाइल्स के आयात-निर्यात के लिए उपयोग करते हैं।

## How to: (कैसे करें:)
```Arduino
// एक सरल CSV पढ़ने और लिखने का उदाहरण
void setup() {
  Serial.begin(9600);
  // डेटा प्रिंट करना
  Serial.println("id,name,value");
  Serial.println("1,Raju,20");
  Serial.println("2,Amit,30");
}

void loop() {
  // कोई loop कार्यक्रम इस उदाहरण के लिए नहीं
}
```
सैंपल आउटपुट:
```
id,name,value
1,Raju,20
2,Amit,30
```
## Deep Dive (गहन जानकारी)
CSV का प्रयोग सन् 1972 से हो रहा है, और यह डेटा के सरल स्वरूपण के लिए प्रसिद्ध है। XML और JSON जैसे विकल्प भी मौजूद हैं, लेकिन CSV की सादगी और कम स्टोरेज जरुरत इसे अनेक स्थानों पर पसंदीदा बनाती है। Arduino में, SD कार्ड पर CSV फाइल बनाने और पढ़ने के लिए SD.h लाइब्रेरी के फंक्शन्स का प्रयोग होता है।

## See Also (अधिक जानकारी के लिए)
- Arduino के लिए SD.h लाइब्रेरी का दस्तावेज: https://www.arduino.cc/en/Reference/SD
- CSV पर और विस्तार से जानकारी: https://tools.ietf.org/html/rfc4180
- JSON और XML के साथ CSV की तुलना: https://www.diffen.com/difference/CSV_vs_JSON vs_XML
