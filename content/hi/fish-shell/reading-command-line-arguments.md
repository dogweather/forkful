---
title:                "Fish Shell: कम्प्यूटर प्रोग्रामिंग पर निर्धारित स्थान से बातचीत को पढ़ना"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपको प्रोग्रामिंग में दोस्ताना कमांड लाइन आर्ग्यूमेंट पढ़ने का कोई सबब चाहिए? इस ब्लॉग पोस्ट में हम आपको कुछ मुख्य कारण बताएंगे जो आपको इस काम में विश्वास करने के लिए प्रेरित करेंगे।

## कैसे

```Fish Shell``` कमांड लाइन आर्ग्यूमेंट पढ़ने के तरीकों को देखें:

- ```$argv``` - सभी स्विच और उनके arguments की सूची देता है।
- ```$argv[0]``` - प्रोग्राम का नाम प्रदान करता है।
- ```count $argv``` - arguments की संख्या प्रदान करता है।

### उदाहरण 1:
कमांड: ```demo.fish argument1 argument2 argument3```

कोड:
```
for arg in $argv
    echo $arg
end
```

आउटपुट:
```
demo.fish
argument1
argument2
argument3
```

### उदाहरण 2:
कमांड: ```demo.fish -a -b -c```

कोड:
```
for arg in $argv
    if test $arg = "-a"
        echo "Option -a has been provided."
    else if test $arg = "-b"
        echo "Option -b has been provided."
    else if test $arg = "-c"
        echo "Option -c has been provided."
    else
        echo "Invalid option."
    end
end
```

आउटपुट:
```
Option -a has been provided.
Option -b has been provided.
Option -c has been provided.
```

## गहराई में जाएं

Fish Shell में कमांड लाइन आर्ग्यूमेंट पढ़ना बहुत आसान है। यह आपको अपने स्क्रिप्ट्स में प्रयोग करने और उपयोगकर्ताओं से user input प्राप्त करने में मदद कर सकता है। कई उपयोगी फंक्शन्स जैसे ```string index``` और ```string sub``` कमांड लाइन आर्ग्यूमेंट्स को प्रोसेस करने में मदद करते हैं। आप इसे अपनी खुद की फंक्शन्स में भी इस्तेमाल कर सकते हैं।

उम्मीद है यह ब्लॉग पोस्ट आपको Fish Shell में कमांड लाइन आर्ग्यूमेंट्स पढ़ने के लिए उपयोगी