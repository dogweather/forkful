---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:33:44.616910-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML Parsing यानी HTML कोड को पढ़ना और उसके तत्वों को समझना है। प्रोग्रामर्स ये इसलिए करते हैं ताकि वे वेबसाइटों की सामग्री को प्रोग्रैमैटिक तरीके से पढ़, बदल या सुधार सकें।

## कैसे करें:

PHP में HTML पार्स करने के लिए DOMDocument क्लास का इस्तेमाल करते हैं:

```php
<?php
$dom = new DOMDocument();
@$dom->loadHTML('<!DOCTYPE html><html><body><h1>नमस्ते दुनिया</h1></body></html>');
$h1 = $dom->getElementsByTagName('h1')->item(0);
echo $h1->nodeValue; // आउटपुट: नमस्ते दुनिया
?>
```
यहाँ, `loadHTML()` हमें HTML लोड करने में मदद करता है, और `getElementsByTagName()` से हम किसी विशेष तत्व को निकाल सकते हैं।

## गहराई से जानकारी:

पहले, HTML पार्सिंग ज्यादा मुश्किल थी क्योंकि अलग-अलग ब्राउजर्स अलग-अलग तरीके से HTML को समझते थे। PHP में `DOMDocument` क्लास से इसके स्टैंडर्डाइज़ेशन में मदद मिली। वैकल्पिक तरीके के तौर पर `SimpleXML` और पियर लाइब्रेरीज जैसे `phpQuery` या `QueryPath` भी मौजूद हैं।

पार्सिंग करते वक्त हमें मानक (well-formedness) और वेबसाइट की स्थिरता पर भी ध्यान देना पड़ता है। तो अगर HTML ठीक से संरचित न हो, तो PHP के DOMDocument में लोड करने में गलतियाँ आ सकती हैं। इसलिए, `libxml_use_internal_errors(true);` का इस्तेमाल करके हम गलतियों को अनदेखा कर सकते हैं।

## सम्बंधित स्रोत:

- PHP के DOMDocument: https://www.php.net/manual/en/class.domdocument.php
- PHP SimpleXML परिचय: https://www.php.net/manual/en/book.simplexml.php
- phpQuery GitHub पेज: https://github.com/phpquery/phpquery
- QueryPath प्रोजेक्ट: https://querypath.org/

इस जानकारी के ज़रिए, आप HTML पार्सिंग के बुनियादी और गहराई में नजरिये से समझ सकते हैं और PHP में अपनी प्रोजेक्ट्स के लिए इसे लागू कर सकते हैं।
