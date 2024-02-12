---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
date:                  2024-01-20T17:56:33.276507-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्ग्यूमेंट पढ़ने का मतलब है टर्मिनल से स्क्रिप्ट को आर्ग्यूमेंट्स प्रदान करना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि इससे स्क्रिप्ट की फ्लेक्सिबिलिटी बढ़ती है और यूजर के इनपुट के अनुसार काम कर सकती है।

## How to: (कैसे करें:)
```Fish Shell
# यहाँ एक सिंपल स्क्रिप्ट है जो आर्ग्यूमेंट्स को प्रिंट करती है
for arg in $argv
    echo "आर्ग्यूमेंट: $arg"
end
```
सैंपल आउटपुट:
```
> fish my_script.fish हैलो वर्ल्ड
आर्ग्यूमेंट: हैलो
आर्ग्यूमेंट: वर्ल्ड
```

## Deep Dive (गहराई में जानकारी):
कमांड लाइन आर्ग्यूमेंट्स को पढ़ना शेल स्क्रिप्टिंग का एक महत्वपूर्ण हिस्सा है और यह लिनक्स/UNIX के प्रारंभिक दिनों से किया जा रहा है। Fish Shell, जिसका फुल फॉर्म "Friendly Interactive Shell" है, मॉर्डन शेल्स में एक है जो योजना को आसान बनाता है। जबकि बैश और ज़ेश जैसे विकल्प भी मौजूद हैं, Fish का सिंटेक्स काफी सहज होता है।

`$argv` वेरिएबल उन आर्ग्यूमेंट्स को स्टोर करता है जो स्क्रिप्ट को पास किए जाते हैं। इसके साथ, आप अपने स्क्रिप्ट में लूप्स, कंडीशनल्स और फंक्शंस के अंदर आर्ग्यूमेंट्स का उपयोग कर सकते हैं।

## See Also (इसके अलावा देखें):
- [Fish Documentation](https://fishshell.com/docs/current/index.html) - फिश शेल के ऑफिसियल डॉक्यूमेंटेशन पर जाएँ।
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html) - फिश शेल सीखने के लिए बेसिक ट्यूटोरियल।
- [Learn X in Y minutes: Fish](https://learnxinyminutes.com/docs/fish/) - जल्दी में फिश शेल सीखें।
- [Awesome Fish](https://github.com/jorgebucaran/awesome-fish) - फिश शेल के लिए संसाधनों और प्लगइन्स का एक संग्रह।
