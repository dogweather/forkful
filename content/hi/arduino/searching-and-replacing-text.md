---
title:                "Arduino: टेक्स्ट खोज और प्रतिस्थापन"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्यों 
आपने अर्दुइनो प्रोग्रामिंग को काफी समय से कर रहे होंगे और आपने यह शब्द "search and replace" सुना होगा। यह जानने के लिए आप काफी आश्चर्य जनक होंगे कि यह क्या है और यह आपके लिए कितना महत्वपूर्ण है। यह आर्टिकल आपको समझाएगा कि आपको क्यों "search and replace" करने की आवश्यकता हो सकती है और इससे आपको क्या लाभ हो सकते हैं।

## कैसे करें
```arduino
void setup(){
    String message = "Hello World!";
    message.replace("Hello", "Namaste"); 
    // "search and replace" करें
    Serial.println(message); 
    // "Namaste World!" मुद्रित करें
}
```
ऊपर दिए गए कोड ब्लॉक में हमने अपनी स्ट्रिंग "message" में शब्द "Hello" को खोज और उसे "Namaste" से बदल दिया है। फिर हमने अपना संदेश मुद्रित किया जो अब "Hello World!" की जगह "Namaste World!" है। इस तरह से, हम आसानी से स्ट्रिंग में शब्दों को खोजें और उसे बदल सकते हैं।

## गहराई से जाने
अपनी प्रोग्रामिंग जानकारी को और भी बढ़ाने के लिए, आप "search and replace" और उसके विभिन्न तकनीकों की गहराई में जानकारी हासिल कर सकते हैं। यह एक आर्डिनो प्रोग्रामिंग में उपयोगी टूल है जो आपको स्ट्रिंग में आसानी से शब्दों को खोजने और बदलने की अनुमति देता है। इसलिए, अपनी आर्डिनो प्रोग्रामिंग को और भी अधिक प्रभावी और स्मार्ट बनाने के लिए यह टूल अपनाएं।

## देखें भी
- [String replace() method in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Using Strings in Arduino](https://learn.sparkfun.com/tutorials/using-the-arduinonbspsupplied-nbspstring-class)
- [Arduino String Manipulation](https