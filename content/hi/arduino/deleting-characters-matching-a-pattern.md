---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों? | What & Why?

पैटर्न मैचिंग करके अक्षरों को हटाने का मतलब होता है कि हम एक विशेष रीति के अनुसार अक्षरों को पहचानते हैं और उन्हें डिलीट करते हैं। प्रोग्रामर्स इसे मुख्य रूप से अनवांछित डेटा को हटाने के लिए करते हैं, जो जरूरी प्रसंस्करण को बाधित कर सकता है।

## कैसे करे | How to:

```Arduino
void setup() {
  // Serial Monitor Open
  Serial.begin(9600);
}

void loop() {
  String str = "Hello, Arduino!";
  String patternToRemove = "ll";

  str.replace(patternToRemove, ""); // remove pattern
  Serial.println(str); // Output: "Heo, Arduino!"
  delay(1000);
}
```

उपर्युक्त कोड में, हमने "ll" पैटर्नवाले अक्षरों को हटाया है। सीरियल मोनिटर पर प्रिंट होने वाला आउटपुट "Heo, Arduino!" होगा।

## गहराई में समझें | Deep Dive

पैटर्न मैचिंग को हटाना किसी भी भाषा में महत्वपूर्ण होता है - चाहे वो टेक्स्ट प्रोसेसिंग हो या डाटा क्लीनिंग। इसका इतिहास कंप्यूटर प्रोग्रामिंग के प्रारंभिक दिनों से जुड़ा हुआ है। इसे करने के लिए अन्य विकल्प शामिल हैं Python, R, और Java, लेकिन Arduino इसे एक स्पष्ट मार्ग में करने का एक आसान तरीका प्रदान करता है। अधिकांश प्रोग्रामिंग भाषाओं में, जैसे की `str.replace(patternToRemove, "")` इस्तेमाल करके, हम इसे आसानी से लागू कर सकते हैं।

## और भी देखें | See Also

* [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
* [Arduino Characters Reference](https://www.arduino.cc/reference/en/language/variables/data-types/char/)