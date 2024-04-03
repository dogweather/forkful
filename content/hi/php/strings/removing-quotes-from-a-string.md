---
date: 2024-01-26 03:42:55.690462-07:00
description: "\u0915\u0948\u0938\u0947: \u092F\u0939\u093E\u0901 PHP \u0915\u0947\
  \ \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u092B\u093C\u0902\u0915\u094D\u0936\
  \u0902\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947\
  \ \u0939\u0941\u090F \u090F\u0915 \u0938\u0940\u0927\u093E \u0909\u0926\u093E\u0939\
  \u0930\u0923 \u0939\u0948."
lastmod: '2024-03-13T22:44:52.456757-06:00'
model: gpt-4-0125-preview
summary: "\u092F\u0939\u093E\u0901 PHP \u0915\u0947 \u092C\u093F\u0932\u094D\u091F\
  -\u0907\u0928 \u092B\u093C\u0902\u0915\u094D\u0936\u0902\u0938 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F \u090F\u0915\
  \ \u0938\u0940\u0927\u093E \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0909\u0926\
  \u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\u091F\u093E\u0928\
  \u093E"
weight: 9
---

## कैसे:
यहाँ PHP के बिल्ट-इन फ़ंक्शंस का उपयोग करते हुए एक सीधा उदाहरण है:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // आउटपुट: Hello, she said, Its a fine day!
```

सरल, है ना? यह `str_replace()` फ़ंक्शन स्ट्रिंग से हटाने के लिए एक अर्रे ऑफ कैरेक्टर्स लेता है, जिसमें सिंगल और डबल दोनों कोट्स शामिल हैं।

## गहराई से जांच
PHP के शुरुआती दिनों में, डेवलपर्स को स्ट्रिंग्स में कोट्स के साथ अतिरिक्त सावधानी बरतनी पड़ती थी, विशेष रूप से जब डेटाबेस में डाटा डालते समय। अनुचित रूप से संभाले गए कोट्स SQL इंजेक्शन हमलों की ओर ले जा सकते थे। इसलिए मैजिक कोट्स, एक ऐसी सुविधा थी जो इनपुट डाटा को स्वतः-एस्केप करती थी। यह deprecated हो गई और आखिर में हटा दी गई थी क्योंकि यह खराब कोडिंग प्रथाओं और सुरक्षा मुद्दों को प्रोत्साहित करती थी।

अब, हम `str_replace()` जैसे फ़ंक्शंस या अधिक उन्नत पैटर्न्स के लिए `preg_replace()` के साथ regex का उपयोग करते हैं। यहाँ एक regex उदाहरण है:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

JSON डेटा के लिए, आप `json_encode()` का उपयोग `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` जैसे विकल्पों के साथ कर सकते हैं ताकि आपके कोट्स में अतिरिक्त बैकस्लैशेज से बचा जा सके।

लागू करते समय, किनारे के मामलों पर विचार करें। क्या हो अगर आपकी स्ट्रिंग में कुछ विशेष कोट्स होने चाहिए, जैसे कि कहानी में संवाद या मापन में इंच? संदर्भ मायने रखता है, इसलिए अपने कोट-हटाने को डेटा के इरादा उपयोग के अनुसार दर्जी करें।

## देखें भी
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: SQL इंजेक्शन रोकथाम](https://owasp.org/www-community/attacks/SQL_Injection)
