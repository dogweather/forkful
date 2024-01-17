---
title:                "टेक्स्ट फाइल लिखना"
html_title:           "Arduino: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
लेखन एक टेक्स्ट फ़ाइल क्या है, और प्रोग्रामर इसे क्यों करते हैं? प्रोग्रामर एक टेक्स्ट फ़ाइल लिखकर डेटा को सुरक्षित रखने और प्रोग्राम को और अधिक दूरगामी बनाने के लिए करते हैं।

## कैसे करें:
यहां सामान्य उदाहरण है कि आप कैसे एक टेक्स्ट फ़ाइल लिख सकते हैं:
```arduino
File myFile;
myFile = SD.open("example.txt", FILE_WRITE);
myFile.println("This is a sample text file.");
myFile.close();
```
क्या आपने ध्यान दिया? हमने ```println()``` का उपयोग स्ट्रिंग को फ़ाइल में लिखने के लिए किया है।

## गहराई में:
1. इतिहास के संदर्भ में, पहली इनपुट/आउटपुट कम्प्यूटर उत्पादन प्रणाली (ऊपरी माध्यम वर्कशीट) में कोई भी फ़ाइल को लिखा या पढ़ा गया।
2. अन्य विकल्प, जैसे कि EEPROM और Serial Port भी फाइलों को लिखने के लिए उपयोग किए जाते हैं।
3. कोड दर्ज करने के लिए आपको कुछ तरीकों का उपयोग करना हो सकता है, लेकिन यह उदाहरण रखा गया है। तथापि, यह बहुत अच्छा नहीं है जब आप बहुत बड़े फ़ाइलों को लिखने करने वाले होते हैं। हम ```write()``` सेवा का उपयोग कर सकते हैं। यह अपेक्षाकृत ही बेहतर उपयोग निर्माण अंश है।

## भी देखें:
आप अधिक जानकारी और संबंधित स्रोतों के लिए निम्न लिंकों पर जा सकते हैं:
1. [आर्दुइनो वेबसाइट पर SD Files का स्केच संग्रह](https://www.arduino.cc/en/Tutorial/Files)
2. [आर्दुइनो फ़ाइल्स लाइब्रेरी डॉक्यूमेंटेशन](https://www.arduino.cc/en/Reference/SD)
3. [टेक्स्ट फ़ाइल क्या है? और इसका उपयोग कैसे किया जाता है?](https://www.techopedia.com/definition/28656/text-file)
4. [टेक्स्ट फ़ाइल के बारे में और अधिक जानें।](https://www.geeksforgeeks.org/reading-and-writing-text-files-using-filesystem-in-arduino/)