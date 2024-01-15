---
title:                "स्ट्रिंग को बड़ा अक्षर करना"
html_title:           "Fish Shell: स्ट्रिंग को बड़ा अक्षर करना"
simple_title:         "स्ट्रिंग को बड़ा अक्षर करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप एक फिश शेल प्रोग्रामर हैं, तो आपने कई बार विभिन्न स्ट्रिंग को कैपिटलाइज करने की जरूरत महसूस की होगी। यह कोई भी स्ट्रिंग हो सकती है, जैसे उपयोगकर्ता का नाम, अनुवादित स्ट्रिंग या फाइल नाम। इसलिए, आपको आमतौर पर स्ट्रिंग के प्रथम अक्षर को कैपिटल में बदलने की जरूरत पड़ती है। 

## कैसे करें

फिश शेल में स्ट्रिंग को कैपिटलाइज करने के लिए, आप `string capitalize` कमांड का उपयोग कर सकते हैं। यह कमांड स्ट्रिंग को कैपिटल में बदलता है और नए स्ट्रिंग को प्रिंट करता है। इसका उपयोग करने के लिए, आपको स्पष्ट रूप से स्ट्रिंग के साथ `|` के बाद `string capitalize` टाइप करना होगा।

```
Fish Shell > string capitalize Welcome to fish shell!
Welcome to fish shell!
```

आप अंदर के सभी अक्षरों को कैपिटल में बदलना चाहते हैं तो आप `string uppercase` कमांड का भी उपयोग कर सकते हैं। इसके लिए, आपको बार-बार अक्षर को बदलने की जरूरत नहीं होगी, `string uppercase` कमांड आपके लिए एक पूर्ण स्ट्रिंग कैपिटल कर देगा।

```
Fish Shell > string uppercase welcome to fish shell!
WELCOME TO FISH SHELL!
```

## डीप डाइव

अब जब आप जान गए हैं कि कैसे स्ट्रिंग को कैपिटल में बदला जाता है, आपका अगला प्रश्न हो सकता है कि यह कैसे संभव है। तो, फिश शेल में `string capitalize` और `string uppercase` का ज़िक्र करने से पहले, इस मामले में थोड़ा और विशेष जान लें।

फिश शेल में स्ट्रिं