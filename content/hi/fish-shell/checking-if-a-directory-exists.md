---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:56:31.177328-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी का अस्तित्व जांचने का मतलब है यह पता करना कि कोई फोल्डर उपस्थित है कि नहीं। प्रोग्रामर्स इसे इसलिए करते हैं ताकि एरर्स से बचा जा सके और उचित ऑपरेशंस किये जा सकें।

## How to: (कैसे करें:)
Fish Shell में डायरेक्टरी चेक करने के लिए कोड सैंपल:

```Fish Shell
if test -d /path/to/directory
    echo "Directory exists!" # डायरेक्टरी उपस्थित है!
else
    echo "Directory not found." # डायरेक्टरी नहीं मिली।
end
```

सैंपल आउटपुट:

```
Directory exists!
```
या

```
Directory not found.
```

## Deep Dive (गहराई से जानकारी)
Fish Shell में `test -d` कमांड का उपयोग परंपरा से चला आ रहा है। UNIX-like सिस्टम्स में यह स्थानिक जांच का तरीका है।
अल्टरनेटिव्स में `[ -d /path/to/directory ]` और `[[ -d /path/to/directory ]]` का उपयोग बैश शेल में होता है। Fish Shell अधिक उपयोगी फीचर्स और सार्थक संदेशों के साथ आता है जो डेवलपर्स को डायरेक्टरी की जांच में सहायता करते हैं।

## See Also (देखें भी)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Stack Overflow Fish Tag](https://stackoverflow.com/questions/tagged/fish)
- [Fish Shell GitHub Repository](https://github.com/fish-shell/fish-shell)