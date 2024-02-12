---
title:                "टेस्ट लिखना"
aliases: - /hi/powershell/writing-tests.md
date:                  2024-02-03T19:32:31.081860-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

PowerShell में परीक्षण लिखने का मतलब ऐसी स्क्रिप्ट्स बनाना है जो आपके PowerShell कोड की कार्यक्षमता को स्वतः सत्यापित करती हैं, यह सुनिश्चित करती हैं कि यह उम्मीद के मुताबिक व्यवहार करे। प्रोग्रामर इसे जल्दी बग्स को पकड़ने, कोड रख-रखाव को सरल बनाने, और यह सुनिश्चित करने के लिए करते हैं कि कोड मॉडिफिकेशन मौजूदा कार्यक्षमता को अनजाने में न तोड़े।

## कैसे:

PowerShell में कोई निर्मित परीक्षण ढांचा नहीं है, लेकिन Pester, एक लोकप्रिय तृतीय-पक्ष मॉड्यूल, परीक्षण लिखने और चलाने के लिए व्यापक रूप से इस्तेमाल किया जाता है। यहाँ आपके PowerShell फंक्शन्स का परीक्षण करने के लिए Pester के साथ शुरू करने का तरीका है।

सबसे पहले, यदि आपने पहले से Pester इंस्टॉल नहीं किया है तो इसे इंस्टॉल करें:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

अगला, माना आपके पास एक सरल PowerShell फंक्शन है जिसे आप परीक्षण करना चाहते हैं, `MyFunction.ps1` के रूप में सेव किया गया:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

इस फंक्शन को Pester के साथ परीक्षण करने के लिए, `MyFunction.Tests.ps1` नाम की एक परीक्षण स्क्रिप्ट बनाइए। इस स्क्रिप्ट में, Pester के `Describe` और `It` ब्लॉकों का उपयोग करके परीक्षण मामलों को परिभाषित करिए:

```powershell
# Import the function to test
. .\MyFunction.ps1

Describe "Get-MultipliedNumber tests" {
    It "Multiplies number by 2 when no multiplier is provided" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "Correctly multiplies number by given multiplier" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

परीक्षणों को चलाने के लिए, PowerShell खोलें, अपनी परीक्षण स्क्रिप्ट से युक्त निर्देशिका में नेविगेट करें, और `Invoke-Pester` कमांड का उपयोग करें:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

नमूना आउटपुट इस प्रकार दिखेगा, जो दर्शाएगा कि आपके परीक्षण पास हुए हैं या विफल हुए हैं:

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\path\to\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0 NotRun: 0
```

यह आउटपुट दिखाता है कि दोनों परीक्षण पास हो गए हैं, आपको विश्वास दिलाते हैं कि आपका `Get-MultipliedNumber` फंक्शन आपके द्वारा परीक्षण किए गए परिदृश्यों के तहत उम्मीद के मुताबिक व्यवहार करता है।
