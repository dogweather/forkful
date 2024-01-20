---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

यादृच्छिक संख्या उत्पन्न करना मतलब है किसी अनिश्चित, अपास्टि संख्या का उत्पादन। प्रोग्रामर इसे तार्किक निर्णय लेने, परिवेश के आधार पर अलग-अलग परिणाम प्राप्त करने या परीक्षण के लिए करते हैं।

## कैसे:

Arduino में, आप यादृच्छिक संख्या उत्पन्न करने के लिए `random()` फ़ंक्शन का उपयोग कर सकते हैं।

```Arduino
void setup(){
  Serial.begin(9600);
}

void loop(){
  int randomNumber = random(0, 101); // generates a number between 0 and 100
  Serial.println(randomNumber);

  delay(1000);
}
```

इस कोड का आउटपुट हर सेकंड में 0 से 100 के बीच की यादृच्छिक संख्या होगी।

## गहराई में जाने:

'random' फ़ंक्शन को 1983 में कसमस गरंनी ने INGRES प्रोजेक्ट के लिए लिखा था। यह `randomSeed(analogRead(0))` के साथ उपयोग किया जा सकता है ताकि हर बार अलग अवधियां उत्पन्न करें। यादृच्छिकता को बेहतर बनाने के लिए `noise()` फ़ंक्शन का भी उपयोग किया जा सकता है, जो परम्परागत `random()` फ़ंक्शन से भिन्न होता है।

## इसे भी देखें:

Arduino एलेमेंटरी पर यादृच्छिक संख्याएँ: [Arduino elementary random numbers](https://www.arduino.cc/en/tutorial/random)
Arduino 'randomSeed' फ़ंक्शन: [Arduino randomSeed function](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
Perlin 'Noise' फ़ंक्शन: [Perlin Noise function](https://mrl.nyu.edu/~perlin/noise/)