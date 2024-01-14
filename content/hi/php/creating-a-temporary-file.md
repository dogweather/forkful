---
title:                "PHP: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों
हेलो दोस्तों, आज हम PHP programming ब्लॉग पोस्ट के साथ हैं और आज हम आपको बताएंगे की ताजगी फाइल क्यों बनाई जाती है।

## कैसे
ताजगी फाइल बनाने के लिए सबसे पहले हमें ```tempnam()``` फंक्शन का उपयोग करके temporary file का नाम जनरेट करना होता है। इसके बाद हम ```fopen()``` फंक्शन का उपयोग करके temporary file को खोलते हैं और उसमे कुछ डेटा लिखते हैं। अंत में हम ```fclose()``` फंक्शन का उपयोग करके ताजगी फाइल को बंद करते हैं।

इस सबको समझाने के लिए हम नीचे दिए गए कोड ब्लॉक का उपयोग करेंगे।

```PHP
<?php

// Generate temporary file name using tempnam() function
$temp_file = tempnam('/tmp', 'temp_');

// Open temporary file using fopen() function
$handle = fopen($temp_file, 'w');

// Write some data to the temporary file
fwrite($handle, 'Hello World! This is a temporary file.');

// Close the temporary file
fclose($handle);

// Output the temporary file name
echo "Temporary file created at: " . $temp_file;
```

यह कोड चलाने के बाद आपको कुछ इस तरह का आउटपुट मिलेगा:

```
Temporary file created at: /tmp/temp_q0h1bo
```

## डीप डाइव
अब हम इस ताजगी फाइल के और गहराई से जानेंगे। ताजगी फाइल का उपयोग कुछ स्थितियों में बहुत ही उपयोगी हो सकता है। जैसे की कभी-कभी हमें अपने कोड में विशेष प्रकार का काम करना होता है जिसका उपयोग आम प्रकार से नहीं किया जा सकता है। ऐसे में हम एक ताजगी फाइल बनाकर उसमे अपने कोड को लिख सकते हैं और उसका उपयोग कर सकते हैं। एक बार काम पूरा हो जाने के बाद यह फाइल खुद बन्द हो जाती है और आपको चिंता करने की आवश्यकता नहीं हो