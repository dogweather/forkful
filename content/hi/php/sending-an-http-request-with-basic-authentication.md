---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:02:55.491563-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
HTTP अनुरोध भेजना और बेसिक प्रमाणीकरण का उपयोग करना आपके एप्लिकेशन को सर्वर से संसाधनों की मांग करने देता है, जहाँ उपयोगकर्ता नाम और पासवर्ड आवश्यक होते हैं। प्रोग्रामर्स इसका उपयोग सुरक्षित API और सेवाओं तक पहुँचने के लिए करते हैं।

## कैसे करें? (How to:)
```php
<?php
$user = 'मेरा_उपयोगकर्ता_नाम';
$password = 'मेरा_पासवर्ड';
$url = 'https://example.com/प्रमाणित-संसाधन';

$context = stream_context_create([
    'http' => [
        'header' => 'Authorization: Basic ' . base64_encode("$user:$password")
    ]
]);

$response = file_get_contents($url, false, $context);
if ($response === FALSE) {
    die('प्रामाणिकता त्रुटि या अन्य त्रुटि');
}

echo $response; // सर्वर से प्राप्त सामग्री को प्रिंट करें
?>
```
सैंपल आउटपुट:
```
सर्वर से प्राप्त प्रतिक्रिया।
```

## गहन अवलोकन (Deep Dive)
HTTP बेसिक प्रमाणीकरण 1990 के दशक से उपयोग में है, एक सिंपल लेकिन स्पष्ट प्रमाणन पद्धति प्रदान करता है जिसमें यूजरनेम और पासवर्ड बेस64 कोडिंग में शामिल होते हैं। हालांकि, यह HTTPS के साथ उपयोग नहीं किए जाते हुए असुरक्षित है क्योंकि क्रेडेंशियल्स सादे टेक्स्ट में इनक्रिप्ट नहीं होते। ऑल्टरनेटिव्स में OAuth और API कीस शामिल हैं जो अधिक सुरक्षित हैं। PHP में cURL और HTTP PECL एक्सटेंशन जैसे विकल्प भी हैं जो और भी उन्नत फीचर्स प्रदान करते हैं।

## संबंधित सूत्र (See Also)
- PHP मैन्युअल पर बेसिक प्रमाणीकरण के बारे में जानकारी: [PHP: HTTP context options](https://www.php.net/manual/en/context.http.php)
- PHP cURL के बारे में अधिक जानकारी: [PHP: cURL](https://www.php.net/manual/en/book.curl.php)
- सुरक्षित एपीआई ऑथेंटिकेशन के लिए OAuth का परिचय: [OAuth 2.0](https://oauth.net/2/)
