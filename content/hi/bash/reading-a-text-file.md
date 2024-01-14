---
title:                "Bash: एक पाठ फ़ाइल को पढ़ना"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

तो आप An hindi reader हैं और आपने text file को कभी खोला है, आप जाहिर कर सकते हैं कि यह एक अभिभावक नजरिए से काफी भयानक दिखता है। लेकिन text file के साथ खेलने का आनंद उसके आंतरिक स्रोतों को समझने में है जो आपको programming की दुनिया में आगे बढ़ते रहने में मदद करेंगे। इसलिए, text file को read करने के बारे में अधिक जानकारी प्राप्त करने से आपको काफी लाभ हो सकता है।

## कैसे

आप Bash programming में हैं या फिर आपने इसका अभ्यास किया है, तो आप जानते ही होंगे कि text file को read करने के लिए `cat` command का उपयोग किया जाता है। ये command file को पढ़कर उसकी सारी जानकारी को terminal में प्रदर्शित करता है। इसके बाद आप file को पढ़ने के लिए `less` command का भी उपयोग कर सकते हैं। इसके साथ साथ, आप `grep` command का भी उपयोग करके text file में विशिष्ट शब्दों या पैटर्न को ढूंढ सकते हैं।

```Bash
# cat command का उपयोग फ़ाइल को पढ़ने के लिए करें
cat filename.txt

# less command का उपयोग फ़ाइल को पढ़ने के लिए करें
less filename.txt

# grep command का उपयोग शब्दों या पैटर्न को ढूंढने के लिए करें
grep pattern filename.txt
```

इन commands के साथ साथ, आप `while` loop का भी उपयोग करके text file के सारे लाइनों पर काम कर सकते हैं। इसके लिए, आपको `read` command से text file से जानकारी पड़नी होगी।

```Bash
# while loop का उपयोग तकनीकी शब्दों को text file में ढूंढने के लिए करें
while read line
do
    command
done < filename.txt
```

## डीप डाइव

Text file को read करने के लिए कई commands और