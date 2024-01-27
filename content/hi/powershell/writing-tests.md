---
title:                "परीक्षण लिखना"
date:                  2024-01-19
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेस्ट लिखना सॉफ्टवेयर के विशेष भागों की जांच और पुष्टि करने की प्रक्रिया है। प्रोग्रामर्स इसलिए टेस्ट लिखते हैं ताकि वे आत्मविश्वास से कोड में बदलाव कर सकें और सुनिश्चित कर सकें कि कोड सही तरीके से काम करता रहे।

## How to: (कैसे करें:)
```PowerShell
# मान लेते हैं कि आपके पास एक सिंपल फ़ंक्शन हैं जो संख्याओं का योग करता है
function Add-Numbers($num1, $num2) {
    return $num1 + $num2
}

# अब हम इस फंक्शन के लिए एक टेस्ट लिखेंगे
Describe "Add-Numbers Tests" {
    It "adds two numbers correctly" {
        Add-Numbers 5 10 | Should -Be 15
    }
}

# टेस्ट चलाने के लिए Pester module की ज़रूरत होती है, इसे इस प्रकार इनस्टॉल करें
Install-Module -Name Pester -Force -SkipPublisherCheck

# अब टेस्ट को चलाएं
Invoke-Pester
```

परिणाम (Sample Output):
```
Starting discovery in 1 files.
Discovery finished in 58ms.
[+] /path/to/your/tests/Add-Numbers.Tests.ps1 101ms (96ms|5ms)
Tests completed in 101ms
Tests Passed: 1, Failed: 0, Skipped: 0 NotRun: 0
```

## Deep Dive (गहराई से समझिए)
टेस्टिंग का इतिहास 1950 के दशक से है जब पहली बार बग्स और गलतियां ढूँढने के लिए सिस्टमेटिक तरीकों का उपयोग हुआ।

PowerShell में `Pester` एक पॉपुलर टेस्टिंग फ्रेमवर्क है। विकल्प के रूप में `NUnit` और `xUnit` जैसे अन्य फ्रेमवर्क भी हैं, हालांकि वे ज्यादातर C# जैसी अन्य भाषाओं के लिए हैं।

Pester में `Describe` ब्लॉक कोड की एक "सुट" के रूप में काम करता है, और `It` ब्लॉक का इस्तेमाल तब होता है जब कोई विशेष टेस्ट केस बताना हो।

## See Also (इसे भी देखें:)
- [Pester](https://pester.dev/docs/quick-start) - Pester का ऑफिसियल डॉक्यूमेंटेशन।
- [PowerShell Testing](https://github.com/pester/Pester/wiki/PowerShell-Testing) - GitHub पर Pester से जुड़ी संसाधन सूची।
