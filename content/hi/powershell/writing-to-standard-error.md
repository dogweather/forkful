---
title:                "मानक त्रुटि में लिखना"
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Standard Error (stderr) वो जगह है जहां PowerShell गलतियों की जानकारी भेजता है। Programmers इसे इस्तेमाल करते हैं ताकि वे सामान्य output और error messages को अलग कर पाएं, जिससे debugging आसान हो जाती है।

## How to (कैसे करें):
```PowerShell
# स्टैंडर्ड आउटपुट पर लिखना (यह सामान्य है)
Write-Host "Hello, World!"

# स्टैंडर्ड एरर पर लिखना
Write-Error "Oops! Something went wrong." 2>&1

# स्टैंडर्ड एरर को फाइल में रीडायरेक्ट करना
Write-Error "This error will be logged." 2> errorlog.txt
```

Sample Output:
```
Hello, World!
Oops! Something went wrong.
```

## Deep Dive (गहराई से जानकारी):
स्टैंडर्ड आउटपुट (stdout) और स्टैंडर्ड एरर (stderr) की अवधारणा Unix लाया था। PowerShell में, `Write-Error` cmdlet स्टैंडर्ड एरर पर लिखता है। आप `2>&1` का उपयोग करके स्टैंडर्ड आउटपुट और एरर को मर्ज भी कर सकते हैं। इसके विकल्प में, `$Error` variable का प्रयोग करके आप अपने स्क्रिप्ट के भीतर एरर्स को ट्रैक कर सकते हैं।

## See Also (और भी देखें):
- [Write-Error Cmdlet - Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error?view=powershell-7.1)