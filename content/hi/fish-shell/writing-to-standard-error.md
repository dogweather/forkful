---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"

category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
मानक त्रुटि आपके प्रोग्राम की गलतियों और चेतावनी मैसेजों को डिस्प्ले करता है। प्रोग्रामर इसे इसलिए यूज़ करते हैं ताकि वे ऑउटपुट और एरर दोनों को अलग-अलग हैंडल कर सकें।

## How to: (कैसे करें:)
```Fish Shell
echo "यह एक मैसेज है" >&2
```
इसका आउटपुट स्टैंडर्ड एरर पर दिखेगा। 

अब, यदि आप चाहें तो स्टैंडर्ड आउटपुट और एरर दोनों को फाइल में रीडायरेक्ट कर सकते हैं:
```Fish Shell
echo "यह नॉर्मल मैसेज है" > output.txt
echo "यह एरर मैसेज है" >&2 2> error.txt
```
पहला मैसेज 'output.txt' में जाएगा और दूसरा 'error.txt' में।

## Deep Dive (गहराई में जानकारी):
स्टैंडर्ड एरर (stderr) का प्रयोग शुरू से ही यूनिक्स सिस्टम्स में एरर मैसेजेज के लिए किया जाता है। इससे प्रोग्राम का ऑउटपुट और एरर मेसेज अलग हो जाते हैं, जिससे डिबगिंग आसान होती है। Fish Shell में `>&2` का प्रयोग करके stderr में लिखा जाता है। एल्टरनेटिव्स में `tee` कमांड, पाइपिंग, और लॉगिंग टूल्स शामिल हो सकते हैं।

## See Also (और जानकारी के लिए):
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Unix Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
- [Redirection Tutorial](https://fishshell.com/docs/current/tutorial.html#tut_redirection)
