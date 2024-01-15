---
title:                "अस्थायी फ़ाइल बनाना"
html_title:           "PHP: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि PHP में अस्थायी फ़ाइल को क्यों बनाया जाता है? यहां हम इस सवाल का जवाब देंगे और आपको बताएंगे कि अस्थायी फ़ाइल कैसे बनाएं और इसके अंदर की गहराई क्या है। 

## कैसे करें

कोडिंग का उदाहरण और नमूना आउटपुट के साथ निम्नलिखित एक चिंतामणि फ़ाइल को बनाएं: 

```PHP
<?php
// अस्थायी फ़ाइल को नई फ़ाइल से बनाएं
$temp_file = tmpfile();
echo $temp_file; // Output: Resource id #1
// अस्थायी फ़ाइल का दूसरा उपयोग - विशेषज्ञता वाले अस्थायी फ़ाइल को बनाएं
$temp_file = tmpfile();
fwrite($temp_file, "यह एक विशेषज्ञता वाली अस्थायी फ़ाइल है।");
echo fread($temp_file, 1024); // Output: यह एक विशेषज्ञता वाली अस्थायी फ़ाइल है।
// समय सीमा लगाकर अस्थायी फ़ाइल को बंद करें
$temp_file = tmpfile();
$temp_file_name = stream_get_meta_data($temp_file)['uri'];
echo $temp_file_name; // Output: C:\xampp\tmp\phpE21.tmp 
fclose($temp_file); // Output: क्या समय सीमा लगाकर अस्थायी फ़ाइल को बंद करें? यहां हमारा फ़ाइल बंद हो गई है।
?>
```

## गहराई

अस्थायी फ़ाइल PHP में काम करने का सबसे अच्छा तरीका है। यह प्रोग्रामिंग दुनिया में खास तौर पर अन्य भाषाओं को अस्थायी फ़ाइल का उपयोग नहीं करने से भिन्न बनाता है। यह अस्थायी फ़ाइल प्रोसेसिंग करता है, बिना कठिनाई के लिए उत्तरदायी होती है और यह मूल शेष फ़ाइल को अस्पष्ट तरीके से साफ़ कर सकत