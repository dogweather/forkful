---
title:                "एक वेब पृष्ठ डाउनलोड करना"
html_title:           "PHP: एक वेब पृष्ठ डाउनलोड करना"
simple_title:         "एक वेब पृष्ठ डाउनलोड करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

PHP के लिए एक वेब पृष्ठ डाउनलोड करने का तरीका

## क्या और क्यों? 
वेब पृष्ठ डाउनलोड करना क्या है और प्रोग्रामर्स इसे क्यों करते हैं? वेब पृष्ठ डाउनलोड करना एक मेहनत से भरे काम हो सकता है, लेकिन इससे हमें बहुत सारी महत्वपूर्ण सूचनाएं प्राप्त होती हैं, जैसे कि वेबसाइट के संरचना, सामग्री और अन्य जानकारी। हम PHP का उपयोग करके वेब पृष्ठ डाउनलोड कर सकते हैं और उसी समय उस पृष्ठ को प्रोसेस कर सकते हैं। 

## कैसे करें? 
```
<?php
  $url = 'https://www.example.com';
  $page = file_get_contents($url);
  echo $page;
?>
```

यह सरल प्रोग्राम में हमने file_get_contents() फ़ंक्शन का उपयोग करके एक URL से पृष्ठ डाउनलोड किया है और उसे प्राप्त की है। फिर हम उस पृष्ठ को echo फ़ंक्शन का उपयोग करके प्रिंट किया है। जो पृष्ठ आएगा वह यह होगा:
```
<html>
<head>
  <title>Example Domain</title>
</head>
<body>
  <h1>Example Domain</h1>
  <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
</body>
</html>
```

और यदि हम file_get_contents() के बजाय cURL extension का उपयोग करना चाहते हैं तो हम इस तरह से कोड कर सकते हैं:
```
<?php
  $url = 'https://www.example.com';
  $ch = curl_init();
  curl_setopt($ch, CURLOPT_URL, $url);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
  $page = curl_exec($ch);
  curl_close($ch);
  echo $page;
?>
```

## गहराई में 
वेब पृष्ठ डाउनलोड करना काफी पुरानी तकनीक है और पर्याप्त जानकारी प्राप्त करने के लिए आप इसे अलग-अलग तरीके से भी कर सकते हैं। मैंने ऊपर बताया है कि कैसे हम file_get_contents() और cURL extension का उपयोग करके डाउनलोड कर सकते हैं। छोटे परियोजनाओं में सामान्यतः file_get_contents() का ही उपयोग करा जाता है जबकि बड़े परियोजनाओं में cURL extension ज्यादातर पसंद किया जाता है। साथ ही आप cURL extension के साथ अनुकूलित और अनेक अन्य फीचर्स का भी उपयोग कर सकते हैं। 

## जारी रखें 
आप इस लेख में बताये गए सूत्रों का उपयोग कर आप PHP में वेब पृष्ठ डाउनलोड कर सकते हैं। अगर आप ज्यादा जानना चाहते हैं तो आप यहां से और जानकारी प्राप्त कर सकते हैं: 

https://www.php.net/manual/en/function.file-get-contents.php 

https://www.php.net/manual/en/curl.examples-basic.php