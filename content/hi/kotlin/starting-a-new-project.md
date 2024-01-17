---
title:                "एक नया प्रोजेक्ट शुरू करना"
html_title:           "Kotlin: एक नया प्रोजेक्ट शुरू करना"
simple_title:         "एक नया प्रोजेक्ट शुरू करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

एक नई परियोजना शुरू करना होता है क्या? इसका मतलब है कि आप नई और रोचक कोडिंग काम से जुड़े हो सकते हैं। इसके आसान, मजेदार और चुनौतीपूर्ण होने के कारण, प्रोग्रामर्स इसे करते हैं।

## कैसे करें:

```Kotlin 
fun main() {
    println("नई परियोजना शुरू करने के लिए कोड कैसे लिखें?")
    // आप जानना चाहते हैं न कि कोणसा तरीका अधिक बेहतर है, यहां हम आपको दो तरीके दिखाएंगे।
 
    //तरीका 1: 
    val projectOne = Project(1, "नई परियोजना");
    println(projectOne.name);
 
    //तरीका 2:
    val projectTwo = Project(id=2, name="नई परियोजना");
    println(projectTwo.id + projectTwo.name);
}
```

आउटपुट: 
```
नई परियोजना शुरू करने के लिए कोड कैसे लिखें? 
नई परियोजना 
तरीका 2 संख्या 2 नई परियोजना
```

## डीप डाइव:

इसके अतिरिक्त, प्रोग्रामर्स को ढाँचा, लाइब्रेरी के आवश्यकता, एप्लिकेशन है, जो नई परियोजना शुरू करने के लिए महत्वपूर्ण है। वे भी सक्रिय रूप से काम करते हैं, और नई और बेहतर तकनीकों और विशेषताओं को अपनाते हैं जो उन्हें अपने प्रोजेक्ट्स में शातिर करते हैं।

## देखें भी:

https://kotlinlang.org/docs/tutorials/getting-started.html
https://www.jetbrains.com/help/idea/creating-and-running-your-first-kotlin-application.html