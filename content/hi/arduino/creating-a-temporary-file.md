---
title:                "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप कभी एक Arduino प्रोजेक्ट बनाते हुए घटनाओं को सुव्यवस्थित रूप से आगे बढ़ाने के लिए विलंब फ़ाइलें बनाने के बारे में सोचा है? शायद आपको सोचा हो कि यह केवल अनुकूलन के लिए ज़रूरी है, लेकिन आज हम देखेंगे कि एक विलंब फ़ाइल बनाने का अपना महत्व है। 

## कैसे

आइए इस लेख के माध्यम से एक उदाहरण के साथ पता करते हैं कि कैसे एक अस्थायी फ़ाइल बनाई जा सकती है। सबसे पहले, हम एक नई Arduino स्केच बनाएं और स्केच को निम्नलिखित कोड से भरें:

```
Arduino void setup() {
   // कदम 1: नई विलंब फ़ाइल बनाएं
   File temp = File.createTempFile("arduino", ".txt");
   Serial.begin(9600);
   Serial.println("Temp file created successfully!");
}

void loop() {
   // लूप के दौरान अस्थायी फ़ाइल में डेटा लिखें
   if(temp) {
      temp.println("Hello world!");
   }
}
```
जैसा कि आप देख सकते हैं, हमने `createTempFile()` फ़ंक्शन का उपयोग किया है जो "arduino" नाम की एक अस्थायी फ़ाइल बनाता है जिसमें ".txt" रूप दिया गया है। साथ ही, हमने फ़ाइल में "Hello world!" लिखने के लिए `print()` फ़ंक्शन का उपयोग किया है। 

अब हम अपने बोर्ड को कंप्यूटर से जोड़ें और सीरियल मॉनिटर खोलें, तो आपको "Temp file created successfully!" का संदेश दिखेगा। इसके बाद, आपको सेली डिवाइस पर कुछ लंबाई का डाटा दिखाई देता होगा जो "Hello world!" के साथ शुरू होगा। 

## गहराई में जाएं

अब हमें पता चल चुका है कि कैसे हम एक अस्थायी फ