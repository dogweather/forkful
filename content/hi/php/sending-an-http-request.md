---
title:                "हथियार क्रिया को भेजना"
html_title:           "PHP: हथियार क्रिया को भेजना"
simple_title:         "हथियार क्रिया को भेजना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

वैसे तो एक प्रोग्रामिंग भाषा के लिए हम बहुत से अलग-अलग काम कर सकते हैं, लेकिन अगर हम आज बात करें तो आपने कभी न कभी सॉफ्टवेयर या वेब डेवलपमेंट में HTTP request पर अंधेरा तो नहीं ढाला ही होगा। इस लेख में हम आपको बताएंगे कि PHP (the current version) में एक आसान तरीके से HTTP request कैसे भेजा जाता है और आपको विशेष चीजें किन बातों पर ध्यान देना चाहिए।

## कैसे करें

जब कोई भी client या यूजर server को request भेजता है, तो वह सभी डेटा को साथ में एक शामिल करता है। एक HTTP request में सभी डेटा उसके आवश्यक भागों में विभाजित होता है। आप कोई भी specific request अपने कंप्यूटर से PHP script के माध्यम से भेज सकते हो जो आपने बनाया है। उस request और उसके विशेष चाहिए हम आपको नीचे PHP कोड के माध्यम से दिखाएंगे।

```PHP
<?php

$request = 'http://example.com/api'; // Change the URL according to your request
$options = array(
    'http' => array(
        'method' => 'GET'
    )
);
$context = stream_context_create($options);
$data = file_get_contents($request, false, $context); // Send HTTP request
var_dump($data); // Output the response

?>
```

ऊपर के कोड में हमने सबसे पहले अपनी डेटा के लिए एक वैश्विक URL स्थापित की है। फिर हमने एक "options" array बनाई है जो हमारे HTTP request के लिए आवश्यक चीजों को शामिल करती है। इस array में हमने "method" जो कि ये डेटा GET request है डेफाइन करी। फिर हमने stream_context_create() मेथड का उपयोग किया है जो हमारे HTTP request को साथ में भेजता है। अंत में हमने file_get_contents() का उपयोग करके सर्वर से डेटा को प्राप्त किया है। और अंत में हमने प