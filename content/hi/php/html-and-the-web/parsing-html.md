---
title:                "HTML विश्लेषण"
aliases:
- /hi/php/parsing-html.md
date:                  2024-02-03T19:13:31.327412-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
