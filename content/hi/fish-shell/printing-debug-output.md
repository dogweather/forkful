---
title:    "Fish Shell: डिबग आउटपुट प्रिंट करना"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

डीबग आउटपुट को प्रिंट करने का क़ार्य सामान्यतः डेवलपर्स द्वारा समस्याओं को खोजने और ठीक करने के लिए किया जाता है। यह प्रोसेस कोड में सही गणना और लॉजिक को देखने में मदद करता है और दोषों को ढूंढने और सुधारने में समय बचाता है।

## कैसे करें

```Fish Shell
if error
echo $error | cat # प्रिंट किया गया डीबग आउटपुट
end
```

आप `echo` कमांड का उपयोग करके प्रिंट किए गए डीबग आउटपुट को रोक सकते हैं। आप इसे `cat` कमांड के साथ भी जोड़ सकते हैं जो आपको रोकने और पोस्ट प्रिंट आउटपुट को देखने में मदद करेगा।

## गहराई में जाएं

डीबग आउटपुट प्रिंट करने के अन्य तरीकों में `echo` और `cat` के अलावा `set -x` भी है जो Fish Shell में रूट प्रिंट करने को सक्षम करता है। आप विभिन्न ऑपरेशन के बीच में प्रिंट किए गए डीबग आउटपुट को देख सकते हैं और यह आपको समस्या को सुलझाने में मदद कर सकता है।

## इसके अलावा देखें

- [निष्कर्ष नहीं है, बचाव की तकनीक](https://rudixcore.wordpress.com/2017/11/01/fish-shell/)
- [फिश शेल की गहराई में जाने का सबसे आसान तरीका](https://medium.com/@alexwachtel/the-easiest-way-to-get-into-the-depths-of-fish-shell-dde8bd91f431)
- [डीबग आउटपुट को प्रिंट करना - एक उपयोगी काम](https://www.ostechnix.com/print-debug-output-useful-fish-shell/)