---
title:                "Bash: सबस्ट्रिंग निकालना"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्यों

अपनी Bash programming skills को enhance करने के लिए, एक बहुत ही महत्वपूर्ण concept है substring extraction (सबस्ट्रिंग निकालना)। यह आपको लंबे टेक्स्ट strings को कम strings में ट्रीम करने में और आपके programs को optimize करने में मदद करता है। इसलिए, हम इस ब्लॉग पोस्ट में आपको सबस्ट्रिंग निकालना की विस्तृत जानकारी देंगे।

## कैसे करें

अब आपको substring extraction (सबस्ट्रिंग निकालना) करने का तरीका बताते हैं। सबसे पहले, हम इसका syntax देखेंगे:

```Bash
${string:position:length}
```

यहां, "string" आपका original string है, "position" आपकी शुरुआती position है और "length" वह characters की संख्या है जो आपको extract करनी है। यहां कुछ examples हैं:

```Bash
ORIGINAL_STRING="This is a test string"
echo "${ORIGINAL_STRING:0:4}" # output: This
echo "${ORIGINAL_STRING:5:6}" # output: is a t
```

इसके अलावा, आप negative positions का भी इस्तेमाल कर सकते हैं। इनमें से सबसे आसान position है "-1" जो आपको आपके string का आखिरी character देगा। यहां यह कॉड है जो आपको आपके string का आखिरी character निकालेगा:

```Bash
ORIGINAL_STRING="This is a test string"
echo "${ORIGINAL_STRING: -1}" # output: g
```

## गहराई से जाने

सबस्ट्रिंग निकालना (substring extraction) के बारे में अधिक जानकारी के लिए, आपको variables और parameters की घंटावाली जानकारी होनी चाहिए। आप variables के भीतर nested variables और parameters को access कर सकते हैं।

आप हमारे लिए λेकिनग आर्टिकल "LinuxCommand - String Manipulation" पढ़ सकते हैं। इसमें अधिक जानकारी पाने के लिए और उपयोगी उदाहरण दिए गए हैं।

## देखें भी

- [LinuxCommand - String Manipulation](https://linuxcommand.org/lc3_man_pages/string-manipulation.1.html)
- [Bash Substring Expansion](https://www.tldp.org/LDP/abs/html/refcards.html#AEN22634)