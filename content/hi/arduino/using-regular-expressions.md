---
title:                "रेगुलर एक्सप्रेशन्स का प्रयोग"
html_title:           "Arduino: रेगुलर एक्सप्रेशन्स का प्रयोग"
simple_title:         "रेगुलर एक्सप्रेशन्स का प्रयोग"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## आप क्या कर सकते हैं और क्यों?
आप ऋण्मुखी अभिव्यक्ति का उपयोग तो करते ही होंगे, लेकिन क्या आप इसके बारे में सुने हैं? इसका उपयोग यूजरस के द्वारा इनपुट के साथ कुछ विशेषता को मिलाने के लिए और एक प्रोग्रामर की जाँच करने के लिए प्रोग्रामर्स द्वारा किया जाता है।

## कैसे करें:
आप अपने आर्डुईनो स्केच में एक सरल विषयमात्र लिंखी होगी, जो उसके लिए इस विशेषता का उपयोग करेगी। [Serial Monitor](https://www.arduino.cc/reference/en/language/functions/communication/serial/) के माध्यम से इसे उपयोग लौटा सकता हैं।

API: गुणवत्ताओं के साथ मेल का विवरण।
```arduino
#include <regex>
#include <iostream>
using namespace std;

void setup() {
  Serial.begin(9600);
  
  regex re("(\\w+) (\\d+)");
  smatch match;
  
  while (Serial.available() == 0) {
    ;
  }
  
  String data = Serial.readStringUntil('\n');
  
  if (regex_search(data, match, re)){
     cout << "शब्द: " << match[1].str() << endl;
     cout << "संख्या: " << match[2].str() << endl;
  } else{
    cout << "कुछ मिला नहीं!" << endl;
  }
  
}

void loop() {
}
```

उनाजमान फीचर:
- आप मुख्य व्यक्तीओं और हिसाब केकुतुब कर सकते हैं।
- बिना आवश्यक नुकसान के डिटोग्राम का समसर्य करें।
- तुरंत प्रतिसपादन करें।

## गहरायी की खुदाई:
अपनी देखभाल के! अपने द्वारा इसका उपायोग करने का कोई मानस नहीं और आपके पास गहरायी मीठाक के बिना आर्डुईनो के साथ खेले सकते हो। [लेकिन क्या आप कुछ दूसरा कर रये हो?](https://www.arduino.cc/en/Tutorial/Knit) नितांतांतांतांतं! एक वज्ञान आपिलेटीक परीक्षण निकले। फ्रैक कर गएं। यह सब आर्डुईनो के साथ कंजने करने के लिए अपने सनक्षत सुसासि हैं किए जे सेकेंट्रे के मद्धमें सेट। अग्रग्रगग वाश के तह! आप फलतू न्ही मुँझ देंगे!

## इस तरह से देखें।
- <https://github.com/arduino-libraries/Regex>
- <https://www.arduino.cc/en/Tutorial/RegexCharacterSearch>
- <https://github.com/davetcc/arduino_regex>

## Learn more:
- <https://www.arduino.cc/en/Reference/SerialReadStringUntil>
- <https://www.arduino.cc/reference/en/language/functions/communication/serial/readstringuntil/>