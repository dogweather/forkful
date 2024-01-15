---
title:                "एचटीएमएल पार्सिंग"
html_title:           "PHP: एचटीएमएल पार्सिंग"
simple_title:         "एचटीएमएल पार्सिंग"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

पार्सिंग HTML क्यों एक महत्वपूर्ण कौशल है? प्रत्येक वेबसाइट एक HTML पेज के रूप में लोड होता है। जब हम एक वेबसाइट को देखते हैं, तो हम उसके साथ अंतर्निहित HTML कोड को नहीं देखते हैं। पार्सिंग HTML की मदद से, हम इस HTML कोड को समझ सकते हैं और उससे आवश्यक जानकारी निकाल सकते हैं। इसलिए, पार्सिंग HTML यूजर और डेवलपर दोनों के लिए बेहद महत्वपूर्ण है।

## कैसे करें

पार्सिंग HTML करने के लिए, हमें PHP के साथ काम करना होगा। पहले, हम एक कनेक्शन स्थापित करेंगे और उसके बाद HTML स्ट्रिंग को प्रोसेस करेंगे। नीचे दिए गए कोड ब्लॉक में, हम एक बेसिक HTML स्ट्रिंग को पार्स करेंगे और उसका आउटपुट देखेंगे।

```PHP
// HTML स्ट्रिंग
$html = "<!DOCTYPE html>
<html>
<head>
	<title>पार्सिंग HTML का उदाहरण</title>
</head>
<body>
	<h1>नमस्ते!</h1>
	<p>यह एक सादा HTML पेज है।</p>
</body>
</html>";

// simplehtmldom का इस्तेमाल करके पार्सिंग करें
include('simple_html_dom.php');
$htmlDom = str_get_html($html);

// <h1> के लिए पाठ निकालें 
$title = $htmlDom->find('h1', 0)->plaintext;

// <p> के लिए पाठ निकालें
$paragraph = $htmlDom->find('p', 0)->plaintext;

// प्रिंट करें
echo $title; // नमस्ते!
echo $paragraph; // यह एक सादा HTML पेज है।
```

## गहराई में जाएं

HTML पेज पार्स करने के लिए अन्य भी कौशल हैं, जैसे XPath का उपयोग करना। XPath, हमें एक डॉक्यूमेंट के भीतर स्थानों को खोजने और उनका समयग्रहण करने की सुविधा देता है। इसके अलाव