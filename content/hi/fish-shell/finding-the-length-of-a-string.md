---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग की लंबाई का पता लगाना एक तरीका है कि स्ट्रिंग में कितने अक्षर हैं। यहां तक कि कंप्यूटर प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि कई बार उन्हें यह जानने की आवश्यकता होती है कि किसी विशेष स्ट्रिंग में कितने अक्षर हैं। यह उन्हें निर्णय लेने में मदद करता है कि उन्हें क्या करना है।

## कैसे करें:

Fish Shell में, `string length` कमांड का इस्तेमाल करके आप स्ट्रिंग की लंबाई पा सकते हैं। 

```Fish Shell 
set my_string "नमस्ते, दुनिया से!"
echo (string length $my_string)
```

यह स्थापना आपको 17 देगा, जो "नमस्ते, दुनिया से!" में वर्णों की संख्या है।

## गहरा अध्ययन

"string length" कमांड, जो फिश शेल में स्ट्रिंग की लंबाई की गणना करती है, हमेशा से इसकी बुनियादी किट का हिस्सा रही है। वैकल्पिक तरीके जैसे कि अनुक्रमित रूप से प्रत्येक करैक्टर की पहचान किए बिना श्यामा की गणना करने के लिए लिनक्स 'wc' उपकरण का उपयोग करना, हो सकता है, लेकिन वे आमतौर पर अधिक complex होते हैं और विभिन्न कारणों से अस्थिर हो सकते हैं।

## यह भी देखें

1. Fish Shell का अधिकारिक दस्तावेज़ीकरण: [string length](https://fishshell.com/docs/current/commands.html#string-length)
2. stackoverflow पर स्ट्रिंग लंबाई के विकल्पों के बारे में चर्चा: [Finding the length of a string in shell](https://stackoverflow.com/questions/17368067/length-of-string-in-bash)