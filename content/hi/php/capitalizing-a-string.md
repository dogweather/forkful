---
title:    "PHP: स्ट्रिंग कैपिटलाइज करना"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

Capitalizing a string is a common task in PHP programming. It is useful for formatting names, titles, and other text to follow proper capitalization rules. It is also necessary when working with data from external sources, as it may not always be in the desired capitalization format. In this blog post, we will discuss how to capitalize a string in PHP and why it is an important skill for programmers to have.

## कैसे करें

बात करें स्ट्रिंग कैपिटलाइजेशन की, तो पीएचपी में यह काफी आसान है। हम आमतौर पर `ucwords()` फंक्शन का इस्तेमाल करते हैं जो दिए गए स्ट्रिंग की हर शब्द का पहला अक्षर कैपिटल बना देता है। हमारे पास एक स्ट्रिंग है `hello world` जो हम `ucwords()` फंक्शन का इस्तेमाल करके इस तरह से कैपिटलाइज कर सकते हैं:

```PHP
$string = "hello world";
echo ucwords($string);
```

और इसका आउटपुट होगा `Hello World`। अगर हमारे पास एक स्ट्रिंग है जिसमें हाथ-होंठ करके केवल पहला शब्द ही कैपिटल होना चाहिए, तो हम इसके लिए `ucfirst()` फंक्शन का इस्तेमाल कर सकते हैं। उदाहरण के लिए, हमारे पास एक और स्ट्रिंग है `hOW are you` जो हम इस तरह से कैपिटलाइज कर सकते हैं:

```PHP
$string = "hOW are you";
echo ucfirst($string);
```

और इसका आउटपुट होगा `HOW are you`।

## डीप डाइव

अब जब हमने स्ट्रिंग कैपिटलाइजेशन की बुनियादी जानकारी हासिल कर ली है, हम इसे और गहराई से समझेंगे। पूरी दुनिया में, अलग-अलग भाषाएं और उनके नियम होते हैं जो स्ट्रिंग कैपिटलाइजेशन को लेकर अलग-अलग हो सकते हैं। पीएचपी में कैपिटलाइजेशन सम्बंधित फंक्शन भी भिन्न-भिन्न भाषाओं और उनके नियमों के लिए पूर्वनिर्धारित हैं। इसलिए शेष भाग