---
title:                "डायरेक्टरी अस्तित्व की जांच करें"
html_title:           "Kotlin: डायरेक्टरी अस्तित्व की जांच करें"
simple_title:         "डायरेक्टरी अस्तित्व की जांच करें"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों 
किसी ने चेक करने का काम क्यों किया जाएगा कि क्या डायरेक्टरी मौजूद है। यदि आप अपने कोड में फ़ॉल्स उदाहरण से बचना चाहते हैं या नए फ़ोल्डर बनाने से पहले मौजूदा फ़ोल्डरों की जाँच करना चाहते हैं।

## कैसे करें 
कोटलिन में फ़ोल्डर की उपस्थिति की जांच करना बहुत आसान है। नीचे उदाहरण में, हम प्रथम अपने फ़ोल्डर को निर्दिष्ट करते हैं और फिर `exists()` विधि का उपयोग करके उसे जांचते हैं। यदि फ़ोल्डर मौजूद है, तो अपने कोड में आगे बढ़ सकते हैं।

```Kotlin
val folder = File("/path/to/folder")
if (folder.exists()) {
    // perform operations on existing folder
} else {
    // create new folder
}
```

अगर आप सिर्फ फ़ोल्डर के नाम की जाँच करना चाहते हैं, तो आप `isDirectory()` का उपयोग कर सकते हैं। यदि वह फ़ोल्डर है, तो सक्रिय कार्यों को जारी रखने के लिए `directory` रूप में उसे उपयोग किया जा सकता है।

```Kotlin
val folder = File("/path/to/folder")
if (folder.isDirectory()) {
    val directory = folder
    // perform operations on existing directory
} else {
    // create new folder
}
```

## गहराई से जाँच
रफ़्तार से फ़ोल्डर की उपस्थिति की जाँच करना अपने योग्यों में काफ़ी सदन्द नहीं है। असली अंतर `exists()` और `isDirectory()` विधियों में यह है कि `exists()` में आप सिर्फ एक वेरीफ़ाय कर रहे हैं कि क्या फ़ोल्डर मौजूद है, जबकि `isDirectory()` में आप वास्तव में कंक्रीट फ़ोल्डर की उपस्थिति की जाँच कर रहे हैं। इसलिए, समझना क