---
title:    "Rust: स्टैंडर्ड त्रुटि को लिखना"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## क्यों
आजकल दुनिया भर में रस्त कार्य प्रोग्रामिंग का बहुत ज्ञापन है। लेकिन, कम ही लोगों को पता होता है कि उन्हें "स्टैंडर्ड त्रुटि प्रिंट करें " कैसे करें। इस ब्लॉग पोस्ट में, हम आपको बताएंगे कि रस्त प्रोग्रामिंग में "स्टैंडर्ड त्रुटि प्रिंट करें " क्यों अहम है और इसे कैसे किया जाए। 

## कैसे करें
हम रस्त में "स्टैंडर्ड त्रुटि प्रिंट करें " करने की विधि show करेंगे। यह आसान कोडिंग उदाहरणों के साथ दिखाया गया है।

```Rust
fn main() {
    eprintln!("यहाँ हम एरर प्रिंट कर रहे हैं।");
    eprintln!("यह एक और एरर हैं।");
}
```

आउटपुट:
```
यहाँ हम एरर प्रिंट कर रहे हैं।
यह एक और एरर हैं।
```

इस प्रकार, आप अपने कोड में "स्टैंडर्ड त्रुटि प्रिंट करें " का उपयोग कर सकते हैं और अपनी एप्लिकेशन में त्रुटियों को आसानी से सामने ला सकते हैं।

## गहराई में जाएं
अब हम "स्टैंडर्ड त्रुटि प्रिंट करें " के बारे में और गहराई से जानेंगे। जब हम कोड लिखते हैं तो हम अक्सर एरर संदेशों को कॉन्सोल पर प्रिंट करते हैं। तो यह किसी भी कोड का हिस्सा होना बेहतर हो सकता है। इसे बिना एरर संदेशों के हम कोड को पढ़ना कठिन हो सकता है और त्रुटियों को सुधारना भी मुश्किल हो सकता है। इसलिए, हमें रस्त में "स्टैंडर्ड त्रुटि प्र