---
title:                "यामल के साथ काम करना"
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

YAML एक डेटा सीरियलाइज़ेशन फॉर्मेट है, जिसे कॉन्फ़िगरेशन फ़ाइलों और डेटा संचार के लिए आसानी से पढ़ा जा सकता है। प्रोग्रामर्स YAML का उपयोग कॉन्फ़िगरेशन और डेटा संरचनाओं को समझने और उन्हें मैनेज करने के लिए करते हैं।

## How to: (कैसे करें:)

```Fish Shell
# Install a YAML parser, for example 'yq'
fisher install jorgebucaran/fisher
fisher install gazorby/fish-yq

# Read a YAML file
set config (cat config.yaml | yq .)

# Extract a specific value
set user_name (echo $config | yq .user.name)

# Print the extracted value
echo $user_name
```

इस उदाहरण में, पहले `yq` को इनस्टॉल किया जा रहा है, फिर `config.yaml` से `user.name` का मान निकाला जा रहा है और अंत में प्रिंट किया जा रहा है।

## Deep Dive (गहराई में जानकारी)

YAML, "YAML Ain't Markup Language" के लिए है और इसका डेवेलपमेंट 2001 में शुरू हुआ था। इसका मुख्य उद्देश्य ह्यूमन-रीडेबल डेटा सीरियलाइज़ेशन फॉर्मेट प्रदान करना था। JSON और XML इसके विकल्प हैं, पर YAML ज्यादा सीधा और पठनीय होता है। `yq` एक लोकप्रिय कमांड-लाइन टूल है जिसे `jq` से प्रेरित होकर बनाया गया था जो JSON के साथ काम करता है।

## See Also (यह भी देखें)

- [Official YAML website](https://yaml.org)
- [The `yq` GitHub repository](https://github.com/mikefarah/yq)
- [Fish Shell scripting documentation](https://fishshell.com/docs/current/index.html)
- [Fisher plugin manager for Fish Shell](https://github.com/jorgebucaran/fisher)