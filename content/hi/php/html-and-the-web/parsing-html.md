---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:31.327412-07:00
description: "PHP \u092E\u0947\u0902 HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u093E \u092F\u0939 \u0939\u094B\u0924\u093E \u0939\u0948 \u0915\u093F HTML\
  \ \u0926\u0938\u094D\u0924\u093E\u0935\u0947\u091C\u094B\u0902 \u0938\u0947 \u0935\
  \u093F\u0936\u093F\u0937\u094D\u091F \u091C\u093E\u0928\u0915\u093E\u0930\u0940\
  \ \u0928\u093F\u0915\u093E\u0932\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930 \u092F\u0939 \u0915\u093E\u0930\u094D\u092F \u0921\
  \u0947\u091F\u093E \u0928\u093F\u0937\u094D\u0915\u0930\u094D\u0937\u0923, \u0935\
  \u0947\u092C \u0938\u094D\u0915\u094D\u0930\u0948\u092A\u093F\u0902\u0917 \u0915\
  \u094B \u0938\u094D\u0935\u091A\u093E\u0932\u093F\u0924 \u092C\u0928\u093E\u0928\
  \u0947 \u092F\u093E\u2026"
lastmod: '2024-03-13T22:44:52.473831-06:00'
model: gpt-4-0125-preview
summary: "PHP \u092E\u0947\u0902 HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\
  \u093E \u092F\u0939 \u0939\u094B\u0924\u093E \u0939\u0948 \u0915\u093F HTML \u0926\
  \u0938\u094D\u0924\u093E\u0935\u0947\u091C\u094B\u0902 \u0938\u0947 \u0935\u093F\
  \u0936\u093F\u0937\u094D\u091F \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E\u0930 \u092F\u0939 \u0915\u093E\u0930\u094D\u092F \u0921\u0947\u091F\
  \u093E \u0928\u093F\u0937\u094D\u0915\u0930\u094D\u0937\u0923, \u0935\u0947\u092C\
  \ \u0938\u094D\u0915\u094D\u0930\u0948\u092A\u093F\u0902\u0917 \u0915\u094B \u0938\
  \u094D\u0935\u091A\u093E\u0932\u093F\u0924 \u092C\u0928\u093E\u0928\u0947 \u092F\
  \u093E\u2026"
title: "HTML \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923"
---

{{< edit_this_page >}}

## क्या और क्यों?
PHP में HTML पार्स करना यह होता है कि HTML दस्तावेजों से विशिष्ट जानकारी निकालना। प्रोग्रामर यह कार्य डेटा निष्कर्षण, वेब स्क्रैपिंग को स्वचालित बनाने या अपने एप्लिकेशन के भीतर विभिन्न वेब पृष्ठों से सामग्री को एकीकृत करने के लिए करते हैं, बिना मैनुअल हस्तक्षेप के कार्यक्षमता को बढ़ाने के लिए।

## कैसे करें:
HTML पार्स करने के लिए, PHP प्रोग्रामर निर्मित-इन फंक्शंस का उपयोग कर सकते हैं या फिर Simple HTML DOM पार्सर जैसे शक्तिशाली लाइब्रेरी पर निर्भर कर सकते हैं। यहाँ, हम PHP के `DOMDocument` और Simple HTML DOM पार्सर का उपयोग करते हुए उदाहरण देखेंगे।

### `DOMDocument` का उपयोग करना:
PHP का `DOMDocument` क्लास इसके DOM एक्सटेंशन का हिस्सा है, जो HTML और XML दस्तावेज नियंत्रण और पार्स करने की अनुमति देता है। यहाँ एक HTML दस्तावेज़ में सभी इमेज को ढूँढने के लिए `DOMDocument` का उपयोग कैसे करें, इसका जल्दी से उदाहरण देखते हैं:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Sample Page</title>
</head>
<body>
    <img src="image1.jpg" alt="Image 1">
    <img src="image2.jpg" alt="Image 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

नमूना आउटपुट:
```
image1.jpg
image2.jpg
```

### Simple HTML DOM पार्सर का उपयोग करना:
अधिक जटिल कार्यों या आसान वाक्य विन्यास के लिए, आप तीसरे पक्ष की लाइब्रेरी का उपयोग करना पसंद कर सकते हैं। Simple HTML DOM पार्सर एक लोकप्रिय विकल्प है, जो HTML संरचनाओं को नेविगेट करने और मैनिपुलेट करने के लिए एक jQuery-जैसा इंटरफेस प्रदान करता है। इसका उपयोग कैसे करें, यह यहाँ है:

पहले, Composer का उपयोग करके लाइब्रेरी को इंस्टाल करें:
```
composer require simple-html-dom/simple-html-dom
```

फिर, उदाहरण के लिए, सभी लिंक्स ढूँढने के लिए HTML को मैनिपुलेट करें:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

यह कोड स्निपेट 'http://www.example.com' की HTML सामग्री को फेच करेगा, इसे पार्स करेगा, और सभी हाइपरलिंक्स को प्रिंट करेगा। जिस URL को आप पार्स करना चाहते हैं उसे `'http://www.example.com'` के स्थान पर बदलना न भूलें।

इन विधियों का उपयोग करके, PHP डेवलपर्स प्रभावी ढंग से HTML सामग्री को पार्स कर सकते हैं, अपनी आवश्यकताओं के अनुसार डेटा निष्कर्षण को अनुकूलित कर सकते हैं, या बिना किसी रुकावट के बाहरी वेब सामग्री को अपने प्रोजेक्ट्स में समाकलित कर सकते हैं।
