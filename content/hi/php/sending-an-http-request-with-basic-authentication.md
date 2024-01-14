---
title:                "PHP: ताकनीकी प्रोग्रामिंग में बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
simple_title:         "ताकनीकी प्रोग्रामिंग में बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी आपको किसी वेबसाइट या एप्लिकेशन से संवाद करने के लिए एक एचटीटीपी अनुरोध भेजना आवश्यक हो सकता है। ऐसे मामलों में, आप एक साधारण प्रमाणीकरण के साथ एचटीटीपी अनुरोध को भेज सकते हैं। यह आमतौर पर सुरक्षाप्रद तरीकों में से एक है जो आपको अपने अनुरोध के साथ सुरक्षा प्रमाणीकरण जोड़ने की अनुमति देता है।

## कैसे करें

```PHP
// हमारी अनुरोध पैकेज से मानक सेटअप प्रमाणीकरण उपलब्ध कराएं
$basicAuth = base64_encode("$username:$password");
$headers = ["Authorization: Basic $basicAuth"];

// हमारे अनुरोध को ट्रिम करें ताकि वहाँ कोई असामान्य रिक्तियां न हों
$url = trim("https://example.com/api/users/123");

// PHP को cURL का उपयोग करके अनुरोध भेजें
$ch = curl_init();
curl_setopt($ch,CURLOPT_URL,$url);
curl_setopt($ch,CURLOPT_HTTPHEADER,$headers);
curl_setopt($ch,CURLOPT_RETURNTRANSFER,1);
$output = curl_exec($ch);

// प्रिंट आउट प्राप्त करें
echo $output;
```

## गाहन जांच

अपने एचटीटीपी अनुरोध के साथ सामान्य प्रमाणीकरण को जोड़कर, आप अपने डेटा को सुरक्षित रख सकते हैं। जब आप अपने अनुरोध को भेजते हैं, तो आपका उपयोगकर्ता नाम और पासवर्ड एन्क्रिप्टेड रूप से भेजा जाता है। सर्वर से प्राप्त डेटा भी असाधारण रूप से एन्क्रिप्टेड होकर वापस आता है। यह आपके संवाद को सुरक्षित बनाने में मदद करता है।

## देखें भी

- [PHP cURL का उपयोग करके एचटीटीपी अनुरोध भेजना](https://www.php.net/manual/en/curl.examples-basic.php)
- [एचटीटीपी बेस