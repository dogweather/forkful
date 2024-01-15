---
title:                "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
html_title:           "PHP: बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

HTTP अनुरोध भेजने के लिए बेसिक प्रमाणीकरण के साथ लोगों को क्यों इंगेज करना चाहिए यह उनकी पहचान को सुरक्षित रखने के लिए है। बेसिक प्रमाणीकरण HTTP सर्वर्स को उपयोगकर्ता के खाते को सत्यापित करने की अनुमति देता है और सुरक्षित संचार सुनिश्चित करता है।

## कैसे

```
<?php

// यूजर और पासवर्ड के साथ एक HTTP रिक्वेस्ट भेजें

$url = 'www.example.com/authenticate';
$username = 'my_username';
$password = 'my_password';

// बेसिक प्रमाणीकरण की खोज करें

$auth = base64_encode($username . ':' . $password);

// रिक्वेस्ट प्रकार सामग्री बनाएं

$data = [
  'name' => 'John Doe',
  'email' => 'john@example.com'
];

$data_string = http_build_query($data);

// फीचर्स सेट करें

$opts = array(
  'http' => array(
    'method' => 'POST',
    'header' => 'Authorization: Basic '. $auth . "\r\n" . 
    'Content-Type: application/x-www-form-urlencoded',
    'content' => $data_string
  )
);

// कनेक्शन खोलें

$context = stream_context_create($opts);

// अनुरोध भेंजें और उत्तर प्राप्त करें

$response = file_get_contents($url, false, $context);

// उत्तर प्रिंट करें

echo $response;
```

जब आप ऊपर दिए गए कोड को निष्पादित करें, आपको सफलतापूर्वक HTTP अनुरोध में बेसिक प्रमाणीकरण के साथ उपयोगकर्ता को सत्यापित करेंगे और आपको साधारण डेटा को उत्तर के रूप में प्राप्त होना चाहिए।

## डीप डाइव

बेसिक प्रमाणीकरण हैडर को सेट करने के लिए, कोड द्वारा इस्तेमाल किए जाने वाले स्ट्रीम कन्टेक्स्ट के माध्यम से HTTP रिक्वेस्ट की सुरक्षा को बढ़ाता है। यह उपयोगकर्ता के द्वारा भेजे गए डेटा को एन्कोड करने और पुनः डिकोड करने में मदद कर