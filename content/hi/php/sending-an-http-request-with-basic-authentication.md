---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

HTTP अनुरोध के साथ मूल प्रमाणीकरण भेजना (sending an HTTP request with basic authentication) कोड के द्वारा सर्वर को डाटा भेजने की प्रक्रिया होती है, जिसमें प्रमाणीकरण भी शामिल होता है। ये क्रिया प्रोग्रामर्स तभी करते हैं जब उन्हें डाटा भेजते समय अपनी पहचान बतानी होती है।

## कैसे: (How to:)

PHP के माध्यम से इसे करना बहुत आसान है। इसका नमूना कोड और आउटपुट निचे दिए गए हैं:

```PHP
<?php
$username = 'user';
$password = 'pass';
$context = stream_context_create(array(
    'http' => array(
        'header'  => "Authorization: Basic " . base64_encode("$username:$password")
    )
));
$result = file_get_contents($url, false, $context);
?>
```
इस कोड का आउटपुट सर्वर पर्वापसीत डाटा होता है, यदि मान्य प्रमाणीकरण खुदाई की गई है।

## गहरी दृष्टि (Deep Dive):

HTTP Basic Authentication का विकास HTTP/1.0 के समय हुआ था और अब भी यह प्रकार का प्रमाणीकरण सर्वरों द्वारा बहुत उपयोग किया जाता है। इसके विकल्पों में OAuth, Digest Access Authentication, etc. शामिल हैं। PHP में, `file_get_contents` के साथ `stream_context_create` का उपयोग HTTP अनुरोध को बनाने के लिए किया जाता है जिसमे Basic Authentication header जोड़ा जाता है।

## अन्य स्रोतों देखें (See Also):

सम्बंधित मुद्दों के इन स्रोतों का अध्ययन करें:

1. PHP `stream_context_create` विवरण: [https://www.php.net/manual/en/function.stream-context-create.php](https://www.php.net/manual/en/function.stream-context-create.php)
 
2. HTTP अनुरोध प्रकरण: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)

3. Basic Authentication के विकल्पों को समझने के लिए OAuth: [https://oauth.net/](https://oauth.net/)