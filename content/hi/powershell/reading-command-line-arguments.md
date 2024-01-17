---
title:                "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट पढ़ना"
html_title:           "PowerShell: कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट पढ़ना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

कमांड लाइन आर्गुमेंटों को पढ़ना यह है कि प्रोग्रामर को आपके स्क्रिप्ट पर अधिक नियंत्रण प्रदान करना है। आप अपने स्क्रिप्ट में भिन्न प्रकार के उपयोगकर्ता इनपुट स्वीकार कर सकते हैं और अपनी पायरेमीटर या स्विच को विभिन्न तरीकों से सेट कर सकते हैं।

## कैसे:

```PowerShell
$args = $args
foreach ($item in $args) {
  Write-Host $item
}
```

इस कोड द्वारा हम उन सभी आर्गुमेंटों का प्रिंट करेंगे जो आपने स्क्रिप्ट को कॉल करते समय पास किए हैं। प्रत्येक आर्गुमेंट एक उपयोगकर्ता द्वारा पेश की गई मान को दर्शाता है जो पूरे नाम में हो सकता है या हमारी स्क्रिप्ट में स्पष्ट नाम का उपयोग कर सकते हैं।

## डीप डाइव:

कमांड लाइन आर्गुमेंटों को पढ़ने का अवधारणा कोम्प्यूटिंग में बहुत पुराना है, लेकिन PowerShell जैसे मजबूत और सुविधाजनक भाषा के आने से इसे बेहतरीन बनाया गया है। आप इसे स्क्रिप्ट के आरंभ में कॉल कर सकते हैं, लेकिन यह भी बंद प्याइपाइप और अन्य फॉर्मैट में आपके स्क्रिप्ट के बीच आकस्मिक सम्बन्धों को थोड़ी और जटिलता प्रदान कर सकता है। यहां आप अपने ही समाधान को बनाने के लिए स्क्रिप्ट में आर्गुमेंट को पढ़ने के लिए कई तरीके हैं और आप प्रोग्रामिंग के आइडियोलॉजी को उसके अंदर लागू कर सकते हैं।

## देखें भी:

आप संबंधित जानकारी के लिए निम्न लिंकों को देख सकते हैं:

- [Microsoft Official Documentation on Reading Command Line Arguments](https://docs.microsoft.com/en-us/powershell/scripting/learn/using-command-line-arguments)
- [PowerShell.org Forums on Reading Command Line Arguments](https://powershell.org/2011/01/31/passing-parameters-to-a-powershell-script/)
- [C# Corner Article on Reading Command Line Arguments in PowerShell](https://www.c-sharpcorner.com/article/working-with-command-line-parameters-in-powershell/)