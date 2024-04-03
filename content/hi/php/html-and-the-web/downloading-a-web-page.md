---
date: 2024-01-20 17:44:43.979478-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PHP \u092E\
  \u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0915\u094B \u0921\u093E\u0909\
  \u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ `file_get_contents()` \u092B\u0902\u0915\u094D\u0936\u0928 \u090F\u0915 \u0938\
  \u0930\u0932 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964."
lastmod: '2024-03-13T22:44:52.475569-06:00'
model: gpt-4-1106-preview
summary: "PHP \u092E\u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0915\u094B\
  \ \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F `file_get_contents()` \u092B\u0902\u0915\u094D\u0936\u0928\
  \ \u090F\u0915 \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964\
  ."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to: (कैसे करें:)
PHP में वेब पेज को डाउनलोड करने के लिए `file_get_contents()` फंक्शन एक सरल तरीका है।

```php
<?php
// वेब पेज का URL
$url = 'http://example.com';

// URL से HTML कंटेंट प्राप्त करना
$html = file_get_contents($url);

// अगर आवश्यक हो, तो आप HTML कंटेंट की प्रोसेसिंग कर सकते हैं
// ...

// HTML कंटेंट दिखाना
echo $html;
?>
```

आउटपुट में `http://example.com` का HTML कोड दिखेगा।

## Deep Dive (गहन जानकारी):
इंटरनेट की शुरुआत से ही वेब पेजेज का डेटा प्राप्त करने का कांसेप्ट रहा है। `file_get_contents()` साधारण और सीधा होने के बावजूद, जटिल जरूरतों के लिए cURL लाइब्रेरी का उपयोग होता है, जैसे कि HTTP headers को सेट करना, POST डेटा भेजना, या एरर हैंडलिंग करना। पीएचपी की cURL लाइब्रेरी पूर्ण-प्रचालक HTTP क्लाइंट है।

पिछले संस्करणों में जहां `fopen()` के साथ रिमोट URL को हैंडल करने के लिए `allow_url_fopen` इनी सेटिंग की आवश्यकता होती थी, वर्तमान में। इसे अधिक सुरक्षित तरीके से करने के लिए cURL का उपयोग पसंद किया जाता है।

## See Also (और देखें):
- PHP Manual on file_get_contents(): https://www.php.net/manual/en/function.file-get-contents.php
- PHP Manual on cURL: https://www.php.net/manual/en/book.curl.php
- HTTP client Guzzle (जो cURL का उपयोग करता है): https://github.com/guzzle/guzzle
