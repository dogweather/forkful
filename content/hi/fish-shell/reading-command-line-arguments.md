---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कमांड लाइन आर्गुमेंट्स पढ़ना एक प्रकार की क्रिया है जिसमें हम टर्मिनल से इनपुट प्राप्त करते हैं। प्रोग्रामर इसे प्रयोग करते हैं ताकि वे अपने कार्यक्रम को अधिक लचीला और उपयोगकर्ता के अनुरूप बना सकें।

## कैसे करें:
```Fish Shell
# आदेश को निम्न तरीके से चलाएँ
> fish myscript.fish आर्गुमेंट1 आर्गुमेंट2

# में माचली स्क्रिप्ट
echo $argv[1] # प्रिंट "आर्गुमेंट1"
echo $argv[2] # प्रिंट "आर्गुमेंट2"
```
सैम्पल आउटपुट:
```
आर्गुमेंट1
आर्गुमेंट2
```

## गहरी गोताखोरी
माचली शेल (Fish Shell) का विकास 2005 में शुरू हुआ था, लेकिन यह पिछले कई दशकों से उपयोग में आ रहे उनिक्स-शैली शेल के एक नवीनीकृत रूप के रूप में देखा जा सकता है। Bash, Zsh और Tcsh जैसे अन्य शेल कमांड आर्गुमेंट्स को पढ़ने का विकल्प प्रदान करते हैं, लेकिन माचली शेल में इसका अधिक सरल और सीधा रूप होता है। `$argv` वारियबल का उपयोग करके माचली शेल में आर्गुमेंट्स पढ़े जाते हैं।

## देखें भी
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Command Line Arguments in Unix/Linux Shell](https://www.baeldung.com/linux/command-line-arguments)
- [Getting Started with Fish Shell](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-the-fish-shell)