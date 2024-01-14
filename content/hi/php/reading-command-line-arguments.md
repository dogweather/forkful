---
title:    "PHP: कमांड लाइन आर्ग्यूमेंट पढ़ना"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## क्यों

कमांड लाइन आर्ग्यूमेंट पढ़ना क्यों ज़रूरी है? सरल शब्द में, इसका मतलब है कि जब आप किसी फाइल को प्रोग्राम कर रहे हो तो आपको कमांड लाइन आर्ग्युमेंट के बारे में ज्ञान होना चाहिए। ये आर्ग्युमेंट आपको प्रोग्राम को दूसरे तरीकों से काम करने की इजाज़त देता है और सुचारू होना हो सकता है। तो अगर आप सीखना चाहते हैं कि यह कैसे काम करता है, तो यह पोस्ट आपके लिए है।

## कैसे करें

आर्ग्युमेंट पढ़ने के लिए, हम `argv` फ़ंक्शन का उपयोग कर सकते हैं। इसके द्वारा हम प्रोग्राम को कुछ परामत्र दे सकते हैं और उससे हमें अपने प्रोग्राम को दूसरे तरीकों से चलाने की इजाज़त मिल सकती है। नीचे एक उदाहरण दिया गया है।

```PHP
<?php
// प्रोग्राम को धारण करने के लिए आर्ग्युमेंट का प्रयोग करें
$argument = $argv[1];

// उपयोगकर्ता को यह संदेश दें
echo "आपका आपस प्यूजा करने का आवेदन स्वीकार किया गया है जैसा कि $argument ।";

// उपयोगकर्ता को स्वागत करें
echo "धन्यवाद!";
?>
```

इस प्रोग्राम को नीचे दिए गए आउटपुट के साथ चलाया जा सकता है।

```bach
php program.php "आप" 
```

आउटपुट:
> आपका आपस प्यूजा करने का आवेदन स्वीकार किया गया है जैसा कि आप।
धन्यवाद!

## गहराई में जाईये

आपने अब आर्ग्युमेंट पढ़ना सीख लिया ह