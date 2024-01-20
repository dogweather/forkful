---
title:                "स्टैंडर्ड त्रुटि पर लिखना"
html_title:           "PowerShell: स्टैंडर्ड त्रुटि पर लिखना"
simple_title:         "स्टैंडर्ड त्रुटि पर लिखना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्टैंडर्ड त्रुटि द्वारा लिखना क्या है, यह क्या करता है? और प्रोग्रामर्स यह क्यों करते हैं? स्टैंडर्ड त्रुटि द्वारा लिखना किसी प्रोग्राम में त्रुटियों को रिपोर्ट करने का एक तरीका है। यह किसी भी त्रुटि को ध्यान से देखने की सुविधा प्रदान करता है और सही त्रुटि से निपटने में मदद करता है।

## कैसे करें:

आप स्टैंडर्ड त्रुटि द्वारा लिखने के लिए इस आसान से PowerShell कमांड का उपयोग कर सकते हैं:

```PowerShell
$ErrorActionPreference = 'Stop'
Write-Error "यह एक त्रुटि है।"
```

इसका उत्पादन निम्न होगा:

```
यह एक त्रुटि है।
श्रेणी: लेखनत्रुटिरूप: System.Management.Automation.RuntimeException
```

## गहराई में जाएं:

स्टैंडर्ड त्रुटि द्वारा लिखने की इस तकनीक का उपयोग सामान्यतः डिबगिंग के समय किया जाता है। इसका उपयोग आसानी से किसी भी प्रोग्राम में त्रुटियों को पता लगाने में मदद करता है। यह एक यादृच्छिक रूप से उपलब्ध विकल्प है और हमेशा कमांड लाइन माध्यम से नहीं चलता। यह स्टैंडर्ड त्रुटि द्वारा लिखने का एक अन्य विकल्प है:

```PowerShell
$ErrorActionPreference = 'Stop'
try {
   Get-Item "C:\doesnotexist.txt"
} catch {
   Write-Error "यह एक त्रुटि है।"
}
```

एक अन्य विकल्प है त्रुटि रिपोर्टिंग के लिए Write-Host फ़ंक्शन का उपयोग करना है:

```PowerShell
Get-Item "C:\doesnotexist.txt" -ErrorVariable outVar
if ($outVar) {
   Write-Host "यह एक त्रुटि है।"
}
```

आप भी अपने स्क्रिप्ट में दिए गए स्कोप में अलग मुद्दे को प्रबंधित कर सकते हैं। इसके लिए आपको डेटा को त्रुटि रिपोर्ट करने के लिए और एक विस्तृत विश्लेषण के लिए विशेष तरीकों का उपयोग करना हो सकता है।

## अधिक जानें:

आप स्टैंडर्ड त्रुटि द्वारा लिखने के बारे में और भी बहुत कुछ जान सकते हैं। और जानकारी के लिए, नीचे दिए गए लिंक को देखें:

- [PowerShell Write-Error](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error?view=powershell-7)
- [PowerShell Write-Host](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-host?view=powershell-7)

स्टैंडर्ड त्रुटि द्वारा लिखना एक उपयोगी और आसान तकनीक है, जो प्रोग्रामर्स को