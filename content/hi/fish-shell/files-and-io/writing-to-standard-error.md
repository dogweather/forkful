---
title:                "मानक त्रुटि के लिए लिखना"
date:                  2024-02-03T19:33:55.216457-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Fish Shell में स्टैंडर्ड एरर (stderr) में लिखने का मतलब है गलती संदेशों या निदान को स्टैंडर्ड आउटपुट (stdout) से अलग निर्देशित करना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि त्रुटि जानकारी को आसानी से पहचाना जा सके, प्रबंधित किया जा सके, या पुनःनिर्देशित किया जा सके, जिससे डीबगिंग और लॉगिंग प्रक्रिया सुगम हो सके।

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