---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फाइल पढ़ना मतलब किसी फाइल से डाटा पढ़ना होता है। प्रोग्रामर्स इसे करने के लिए Bash इस्तेमाल करते है। इसका मुख्य कारण है कि यह सरल और सुगम है।

## कैसे करें:
Bash में टेक्स्ट फाइल कैसे पढ़ें:

```Bash
while read line
do
    echo "$line"
done < input.txt
```
ऊपरी कोड की आउटपुट: हर लाइन आलग आलग दिखेगी।

## गहरी जानकारी
Bash जो UNIX और Linux ऑपरेटिंग सिस्टम्स पर चलने वाली एक shell है, बहुत समय से फाइल मैनेजमेंट में उपयोग होती आ रही है। 

हालांकि इसे विभिन्न तरीकों से किया जा सकता है, जैसे `cat`, `more` या `less` कमांड का उपयोग करके, `read` कमांड बहुत ही सरल और लचीला होता है। 

इस कमांड का उपयोग करने में यदि आपको कोई समस्या आती है, तो आपको `-r` विकल्प का उपयोग करने की आवश्यकता पड़ सकती है। यह `\` को escape करने के लिए होता है।

```Bash
while IFS= read -r line
do
    echo "$line"
done < input.txt
```

## अन्य सूत्र
1. Bash Scripting Guide: https://tldp.org/LDP/abs/html/
2. Bash Manual: https://www.gnu.org/software/bash/manual/bash.html
3. Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/abs-guide.html