---
title:                "Ruby: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों
अगर आप अपने वेब अनुरोध में उपयोगकर्ता की पहचान और सुरक्षा को सम्मिलित करना चाहते हैं, तो आप बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेज सकते हैं। 

## कैसे करें
आप इस तरह से एक HTTP अनुरोध भेज सकते हैं:

```Ruby
require 'rest-client'

url = 'https://www.example.com/api'
headers = {'Accept': 'application/json', 'Authorization': 'Basic dXNlcm5hbWU6cGFzc3dvcmQ='}
response = RestClient.get(url, headers)

puts response.code # 200
```

यहां, पहले हम Rest-Client gem को आवश्यकता अनुप्रयोग के रूप में शामिल करते हैं। फिर, हमें URL की आवश्यकता होती है और सहायक शीर्षक जो हमें हमारे HTTP अनुरोध के साथ साक्षात्कार करने में मदद करता है। अंत में, हमें प्रतिक्रिया प्राप्त करने के लिए Rest-Client क्लास का .get() मेथड कोहना। आप उपयोगकर्ता का नाम और पासवर्ड को बेस-64 अनुमति टोकन में कनवर्ट करने के लिए OpenSSL gem भी उपयोग कर सकते हैं।

## गहराई में जाएँ
बेसिक प्रमाणीकरण HTTP अनुरोध के लिए एक स्टैंडर्ड और साधारण तकनीक है। इसमें, उपयोगकर्ता का नाम और पासवर्ड एक्सेस कोड के रूप में प्रदान किया जाता है। लेकिन, यह एक सुरक्षित विधि नहीं है क्योंकि पासवर्ड हमेशा संपादक या कैश में उपलब्ध होता है। प्रारंभिक सुरक्षा को बढ़ाने के लिए, आप एक उपयोगकर्ता के साथ प्रमाणीकरण टोकन जैसे OAuth या JSON Web Tokens का उपयोग कर सकते हैं। इससे उपयोगकर्ता के पासवर्ड के साथ सुरक्षा पर भरोसा बढ़े