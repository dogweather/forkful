---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:58:13.202974-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

PHP में डायरेक्टरी की जाँच करना यह है कि कोई फोल्डर मौजूद है या नहीं, यह देखना। कार्यक्रमकर्ता इसे फ़ाइलों को सहेजने, उन्हें पढ़ने, या नयी डायरेक्टरी बनाने से पहले जांचते हैं ताकि त्रुटियों से बचा जा सके।

## How to: (कैसे करें:)

```PHP
<?php
// डायरेक्टरी की जाँच
$directory = "/path/to/directory";

// is_dir() फंक्शन का इस्तेमाल करके
if (is_dir($directory)) {
    echo "डायरेक्टरी मौजूद है!";
} else {
    echo "डायरेक्टरी मौजूद नहीं है।";
}

// file_exists() फंक्शन से भी जाँच सकते हैं
if (file_exists($directory) && is_dir($directory)) {
    echo "डायरेक्टरी मौजूद है!";
} else {
    echo "डायरेक्टरी मौजूद नहीं है।";
}
?>
```

सैंपल आउटपुट हो सकता है:
```
डायरेक्टरी मौजूद है!
```
या
```
डायरेक्टरी मौजूद नहीं है।
```

## Deep Dive (गहराई से जानकारी):

डायरेक्टरी की जांच PHP4 के समय से है। `is_dir()` सबसे स्टेंडर्ड तरीका है और `file_exists()` फंक्शन भी फाइलों के साथ-साथ फोल्डरों के लिए काम करता है। `is_dir()` का इस्तेमाल करना ज्यादा स्पष्ट होता है क्योंकि यह सीधे डायरेक्टरी की जाँच करता है जबकि `file_exists()` किसी भी प्रकार के फाइल सिस्टम ऑब्जेक्ट की जाँच कर सकता है। ये तब भी महत्वपूर्ण है जब आप फाइल ऑपरेशन्स कर रहे हों, जैसे कि फाइल अपलोड, या फाइल सिस्टम के माध्यम से डाटा मैनेज करना।

## See Also (और भी देखें):

- PHP Manual on `is_dir()`: https://www.php.net/manual/en/function.is-dir.php
- PHP Manual on `file_exists()`: https://www.php.net/manual/en/function.file-exists.php
- Stack Overflow discussions on directory checks in PHP: https://stackoverflow.com/search?q=php+check+if+directory+exists