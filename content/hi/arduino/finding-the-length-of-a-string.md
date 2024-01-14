---
title:                "Arduino: स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्यों 

क्या आपको पता है कि Arduino में आप स्ट्रिंग की लंबाई कैसे निकाल सकते हैं? अगर नहीं, तो आप उसे जानना चाहते हैं तो आप सही जगह पर हैं। इस आर्टिकल में हम आपको स्ट्रिंग की लंबाई को निकालने के बारे में बताएंगे और कुछ उदाहरण भी देंगे। 

## कैसे करें 

जब हमें कोई भी स्ट्रिंग की लंबाई निकालनी होती है, तो हम दो तरीकों से कर सकते हैं। पहला तरीका है `strlen()` फ़ंक्शन का प्रयोग करना और दूसरा तरीका है अपना खुद का कोड लिखना। हम यहां `strlen()` फ़ंक्शन का इस्तेमाल करेंगे। स्ट्रिंग की लंबाई निकालने के लिए हमें स्ट्रिंग का पता देना होगा और उससे फ़ंक्शन को कॉल करना होगा। नीचे दिए गए कोड ब्लॉक में हम आपको दो स्ट्रिंग्स के उदाहरण देंगे और उनकी लंबाई को निकालेंगे। 

```Arduino
void setup() {
  Serial.begin(9600);
  String name = "John";
  String message = "Hello, World!";
  int nameLength = strlen(name);
  int messageLength = strlen(message);
  Serial.print("Name: ");
  Serial.print(name);
  Serial.print("; Length: ");
  Serial.println(nameLength);
  Serial.print("Message: ");
  Serial.print(message);
  Serial.print("; Length: ");
  Serial.println(messageLength);
}

void loop() {
  
}
```

जब आप ऊपर दिए गए कोड ब्लॉक को अपने Arduino में अपलोड करेंगे, तो आपको निम्नलिखित आउटपुट मिलेगा: 

```
Name: John; Length: 4 
Message: Hello, World!; Length: 13 
``` 


## Deep Dive

स्ट्रिंग की लंबाई निकालने के लिए `strlen()` फ़ंक्शन को C और C++ में पहले से ही उपलब्ध किया गया है। यह फ़ंक्शन एक character array को आधार बनाकर उसकी लंबाई को निकालता है। इसका शुरु