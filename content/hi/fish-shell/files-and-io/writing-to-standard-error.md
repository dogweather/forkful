---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:55.216457-07:00
description: "\u0915\u0948\u0938\u0947: Fish Shell \u092E\u0947\u0902, \u0906\u092A\
  \ \u0905\u092A\u0928\u0947 \u0906\u0909\u091F\u092A\u0941\u091F \u0915\u094B `>&2`\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0915\u0947 stderr \u092E\
  \u0947\u0902 \u0932\u093F\u0916 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u092F\u0939\u093E\u0902 \u090F\u0915 \u092E\u0942\u0932 \u0909\u0926\u093E\u0939\
  \u0930\u0923 \u0939\u0948."
lastmod: '2024-03-13T22:44:53.094936-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u092E\u0947\u0902, \u0906\u092A \u0905\u092A\u0928\u0947 \u0906\
  \u0909\u091F\u092A\u0941\u091F \u0915\u094B `>&2` \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930 \u0915\u0947 stderr \u092E\u0947\u0902 \u0932\u093F\u0916\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0902 \u090F\
  \u0915 \u092E\u0942\u0932 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे:
Fish Shell में, आप अपने आउटपुट को `>&2` का उपयोग कर के stderr में लिख सकते हैं। यहां एक मूल उदाहरण है:

```fish
echo "This is an error message" >&2
```

यह कमांड सिर्फ एक संदेश को stdout के बजाय stderr में गूंजता है। यदि आप एक स्क्रिप्ट लिखने जा रहे हैं जो नियमित और त्रुटि संदेश दोनों को आउटपुट करता है, तो आप कुछ इस तरह कर सकते हैं:

```fish
echo "Starting the process"
echo "An error occurred" >&2
echo "Process completed"
```

अगर आप स्क्रिप्ट चलाते हैं और stderr को एक फाइल में पुनःनिर्देशित करते हैं, तो नमूना आउटपुट इस प्रकार होगा:

```
Starting the process
Process completed
```

स्टैंडर्ड आउटपुट में त्रुटि संदेश नहीं दिखाई देगा लेकिन वह आपके द्वारा stderr को पुनःनिर्देशित की गई फाइल में पाया जाएगा।

अधिक जटिल त्रुटि हैंडलिंग या लॉगिंग की आवश्यकता वाले परिदृश्यों में, Fish में इसके लिए विशेष रूप से अभिप्रायित इन-बिल्ट लाइब्रेरियां नहीं होतीं। हालाँकि, आप बाहरी उपकरणों का उपयोग कर सकते हैं या सहायता के लिए फ़ंक्शन लिख सकते हैं। उदाहरण के लिए, एक सरल लॉगिंग फंक्शन इस तरह दिख सकता है:

```fish
function log_error
    echo $argv >&2
end

log_error "This is an advanced error message"
```

यह फंक्शन `log_error` जो भी स्ट्रिंग आप इसे देंगे उसे stderr में लिख देगा। इस तरह के फंक्शन का उपयोग करने से आपकी त्रुटि हैंडलिंग को साफ और सुसंगत रखने में मदद मिल सकती है।
