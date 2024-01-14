---
title:                "Fish Shell: स्ट्रिंग को बड़े अक्षरों में लिखना"
simple_title:         "स्ट्रिंग को बड़े अक्षरों में लिखना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हम अपनी पाठ को और प्रभावी बनाने के लिए स्ट्रिंग को कैपिटलाइज़ करना चाहते हैं। इस ब्लॉग पोस्ट में हम देखेंगे कि कैसे आप Fish Shell द्वारा स्ट्रिंग को कैपिटलाइज़ कर सकते हैं।

## कैसे करें

```Fish Shell
set str "hello world"
echo $str | tr "[:lower:]" "[:upper:]" # यह स्ट्रिंग को कैपिटलाइज़ करेगा 
```

यहां, हमने `set` कमांड का उपयोग करके स्ट्रिंग को परिभाषित किया और फिर `echo` कमांड के साथ `tr` कमांड का उपयोग करके स्ट्रिंग को कैपिटलाइज़ किया। यहां `tr` `[:lower:]` को `[:upper:]` से प्रतिस्थापित करता है, जिससे हमारा स्ट्रिंग कैपिटल्स में बदल जाता है।

## गहराई में जाएं

स्ट्रिंग को कैपिटलाइज़ करने के लिए Fish Shell में डिफ़ॉल्ट रूप से `tr` कमांड का उपयोग किया जाता है। यह कमांड सभी लोअरकेस अक्षरों को अपरकेस में बदलता है। लेकिन, यदि आप अपनी स्ट्रिंग में अधिक प्रतिबंधित विकल्पों को लेना चाहते हैं, तो आप अन्य विकल्पों का भी उपयोग कर सकते हैं। आप फिश शेल मैनुअल को देख सकते हैं जो इस बारे में अधिक गहराई से बताता है।

## देखें भी

- [Fish Shell डॉक्यूमेंटेशन](https://fishshell.com/docs/current/)
- [अपरकेस किया हुआ स्ट्रिंग प्रिंट करना](https://www.cyberciti.biz/faq/bash-uppercase-string-conversion/)
- [दूसरे शेल में स्ट्रिंग कैपिटलाइज़ करना](https://www.tecmint.com/capitalize-string-lowercase-upper-case-shell-script/)