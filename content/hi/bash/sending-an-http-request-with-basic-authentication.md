---
title:                "बेसिक प्रमाणीकरण के साथ एक Http अनुरोध भेजना"
html_title:           "Bash: बेसिक प्रमाणीकरण के साथ एक Http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक Http अनुरोध भेजना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों 
जब हम किसी वेब साइट से कुछ जानकारी या डेटा को गेट करना चाहते हैं, तो हमें अक्सर उस साइट द्वारा प्रदान किए गए एक खास रोजगार को अदा करना पड़ता है, जो HTTP अनुरोध के माध्यम से होता है, जिसमें आपके शीर्षक स्तर पर प्रमाणीकरण की आवश्यकता हो सकती है। इसलिए, बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना आवश्यक हो सकता है।

## कैसे करें
यदि आप Basic Authentication के साथ HTTP अनुरोध भेजना सीखना चाहते हैं, तो आप निम्न उदाहरण को देख सकते हैं।

```Bash
# पहले, आपको curl पैकेज को अपडेट करना होगा
$ sudo apt update
$ sudo apt install curl

# अब, आपको कुछ प्रमाण पत्रों को तैयार करने की आवश्यकता होगी
$ username="YOUR_USERNAME"
$ password="YOUR_PASSWORD"

# इसके बाद, आपको अनुरोध का URL और डेटा भेजना होगा
$ url="YOUR_URL"
$ data="YOUR_DATA"

# अंत में, आपको curl कमांड के साथ -u फ्लैग का उपयोग करके एचटीटीपी अनुरोध भेजना होगा
$ curl -u "$username:$password" -X POST -d "$data" "$url"
```

यह उदाहरण आपको बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजने के लिए काफी दिखाएगा। आप इसे अपने खुद के उदाहरणों में भी उपयोग कर सकते हैं।

## गहराई में जाने
जब हम HTTP अनुरोध भेजते हैं, तो हमारा अतिरिक्त सुरक्षा सुनिश्चित करने का स्तर Basic Authentication हो सकता है। यह एक आसान और स्पष्ट प्रमाणीकरण का माध्यम होता है, जो उपयोगकर्ता के नाम और पासवर्ड के साथ सर