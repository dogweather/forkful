---
title:                "डिबग ऑउटपुट प्रिंट करना"
html_title:           "Rust: डिबग ऑउटपुट प्रिंट करना"
simple_title:         "डिबग ऑउटपुट प्रिंट करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## आखरी तक बात पर हाँ

## क्या और क्यों? 

डेबग आउटपुट प्रिंट करना एक अहम काम है जो हमारे प्रोग्रामिंग को बेहतर बनाता है। जब हम एक प्रोग्राम लिखते हैं तो हमें विभिन्न चरणों में अपने कोड को डेबग करने की आवश्यकता होती है। इसके लिए हम डेबग आउटपुट का उपयोग करते हैं जो हमें यह बताने के लिए मदद करता है कि कैसे हमारा कोड वर्क कर रहा है और क्या समस्याएं हैं जो हमें सुलझानी होंगी। 

## कैसे करें? 

```Rust
fn main() {
    // This is a simple program to demonstrate debug output in Rust
    let num1 = 10; // Declaring a variable with value 10
    let num2 = 20; // Declaring a variable with value 20
    println!("The value of num1 is {}", num1); // Printing the value of num1
    println!("The value of num2 is {}", num2); // Printing the value of num2
    let sum = num1 + num2; // Adding the two variables
    println!("The sum of num1 and num2 is {}", sum); // Printing the sum
}
```

यहाँ, हमने एक सरल प्रोग्राम बनाकर डेबग आउटपुट का उपयोग किया है। हमने इसमें २ मान डेक्लेयर किये हैं, उन्हें प्रिंट किया है और उन्हें एक साथ जोड़कर उसकी गणना की है। हमने इसके लिए println! मैक्रो का उपयोग किया है जो हमें वर्तमान मान के साथ एक स्ट्रिंग प्रिंट करने की अनुमति देता है। यह एक सरल और अनुकूल पद्धति है जो कोड को समझने में मदद करती है। 

## गहराई में जाएं 

रस्ट प्रोग्रामिंग भाषा में डेबग आउटपुट का उपयोग वास्तव में एक ऐसा चीज है जिसका ज़रूरीता परिणाम में हमें उस बग को ठीक करने के लिए प्रोग्राम को तलाशने में होती है। यह हमारे कोड में छीछोरों (bugs) को पहचानने और उन्हें ठीक करने में मदद करता है। यदि आप अन्य विकल्पों की बात करें, तो आप अपने कोड में println! का उपयोग कर सकते हैं जो कि रस्ट की ही शाखा है। इसके अलावा, आप अन्य उपकरण भी प्रयोग कर सकते हैं जो कि आपको अपने कोड का डेबग करने में मदद करेंगे। आप अपनी जानकारी बढ़ाने के लिए [रस्ट दस्तावेज](https://doc.rust-lang.org/book/) की मदद ले सकते हैं। 

## और जानें 

कृपया [यहाँ](https://doc.rust-lang.org/std/macro.println.html) जाकर रस्ट की println! मैक्रो के बारे में अधिक जानकारी प्राप्त करें।