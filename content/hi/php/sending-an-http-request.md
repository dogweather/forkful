---
title:                "एक http अनुरोध भेजना"
html_title:           "PHP: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

# क्या और क्यों?
HTTP अनुरोध भेजना क्या है और क्यों प्रोग्रामर्स इसे करते हैं पर दो-तीन वाक्यों में बताएं। 

HTTP रेखा  का मतलब है हाइपर टेक्स्ट ट्रांसफर प्रोटोकॉल। यह एक प्रोटोकॉल है जो रायटिंग और ग्राहक-सर्वर संबंधों को सत्यापित करने के लिए इंटरनेट पर इस्तेमाल किया जाता है। कंप्यूटर प्रोग्राम्स ऐसे अनुरोध को भेजते हैं जो तत्काल जमीन पर इस्तेमाल किया जाने वाले भौतिक संसाधनों की चयापचय करते हैं। 

# कैसे करें?
```PHP
// स्मेपल GET अनुरोध को PHP के माध्यम से भेजना।
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "http://www.example.com/"); // यहाँ अपने URL को जोड़ें 
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$output = curl_exec($ch);
curl_close($ch);
echo $output; // यहाँ अपने उत्तर को देखें
```

```PHP
// स्मेपल POST अनुरोध को PHP के माध्यम से भेजना।
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "http://www.example.com/"); // यहाँ अपने URL को जोड़ें 
curl_setopt($ch, CURLOPT_POST, 1);
curl_setopt($ch, CURLOPT_POSTFIELDS, "name=John&city=Delhi"); // अपने डेटा को जोड़ें 
$output = curl_exec($ch);
curl_close($ch);
echo $output; // यहाँ अपने उत्तर को देखें
```

# गहराई में:
HTTP अनुरोध भेजने के पीछे एक उत्पत्ति की गहराई है। 1991 में, टिम बर्नर्स-ली ने HTTP को अपनी रीचसेट प्रोग्राम में शामिल किया, जो एक वन्न विदेश क्यूट्स सिस्टम थी। इससे पहले एक्स.500 अनुरोध प्राप्त करने के लिए आपको ब्राउज़र खोलना और स्टेटकोड पता करना होता था। लेकिन आज, कम्‍प्‍यूटर प्रोग्राम भी अनुरोध भेज सकते हैं और समय, प्रदर्शन व प्रदोषण की व्यापार यान्त्रिकी ने आजकल साथी के बदले के बजाय ऐसे लोअर एचटीटीपी शामिल कर लिए हैं। 

अन्य विकल्प में cURL, Wget और WordPress HTTP API हैं। अलग-थलग स्थितियों में, आपको कुछ लोगों को HTTP अनुरोध भेजने के लिए सबसे सही विकल्प चुनने के लिए आवश्यकता हो सकती है। 

अधिक जानकारी के लिए निम्न जर्नल्स को संदर्भित करें:

- PHP डॉक्यूमेंटेशन: http://php.net/manual/en/index.php
- HTTP रूर्लस: http://www.w3.org/Protocols/
- cURL किसी भी URL को ऊपर स्वीकार नहीं करता, इन और किसी प्रोटोकॉल को भेजने की तकनीक सीखें।

# जारी रखें:
साथ ही, आप cURL FAQ को जांच सकते