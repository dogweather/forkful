---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
aliases:
- hi/powershell/reading-command-line-arguments.md
date:                  2024-01-20T17:57:10.671419-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कमांड लाइन आर्गुमेंट्स पढ़ना यह देखने की प्रक्रिया है कि यूजर ने स्क्रिप्ट चलाते समय क्या इनपुट दिया है। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि इससे स्क्रिप्ट को फ्लेक्सिबल और यूजर-डिफाइंड बहेवियर देने में मदद मिलती है।

## कैसे करें:
PowerShell में कमांड लाइन आर्गुमेंट्स को इस्तेमाल करने का सबसे साधारण तरीका `$args` एरे का उपयोग करना है। आइये देखें:

```PowerShell
# स्क्रिप्ट example.ps1

# प्रिंट सभी आर्गुमेंट्स
$args

# इंडेक्स के हिसाब से विशेष आर्गुमेंट्स प्रिंट करें
"पहला आर्गुमेंट: " + $args[0]
"दूसरा आर्गुमेंट: " + $args[1]
```

जब आप ऊपर वाली स्क्रिप्ट को इस तरह चलाएंगे:

```Shell
powershell .\example.ps1 "नमस्ते" "दुनिया"
```

आउटपुट होगा:

```
नमस्ते
दुनिया
पहला आर्गुमेंट: नमस्ते
दूसरा आर्गुमेंट: दुनिया
```

## गहराई से:
कमांड लाइन आर्गुमेंट्स पढ़ने की क्षमता हर प्रोग्रामिंग भाषा में होती है, और PowerShell में इसके लिए `$args` एरे की शुरुआत से ही व्यवस्था है। `$args` सबसे सरल तरीका है, लेकिन आप advanced parameter binding और param blocks का इस्तेमाल कर सकते हैं जो कि स्क्रिप्ट पैरामीटर्स को और भी फाइन कंट्रोल देते हैं।

## देखने के लिए:
- [About Functions Advanced Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [About Scripting](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)
