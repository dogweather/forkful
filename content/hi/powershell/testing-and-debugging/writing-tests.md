---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:31.081860-07:00
description: "\u0915\u0948\u0938\u0947: PowerShell \u092E\u0947\u0902 \u0915\u094B\
  \u0908 \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u092A\u0930\u0940\u0915\u094D\
  \u0937\u0923 \u0922\u093E\u0902\u091A\u093E \u0928\u0939\u0940\u0902 \u0939\u0948\
  , \u0932\u0947\u0915\u093F\u0928 Pester, \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\
  \u0930\u093F\u092F \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092E\
  \u0949\u0921\u094D\u092F\u0942\u0932, \u092A\u0930\u0940\u0915\u094D\u0937\u0923\
  \ \u0932\u093F\u0916\u0928\u0947 \u0914\u0930 \u091A\u0932\u093E\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F \u0935\u094D\u092F\u093E\u092A\u0915 \u0930\u0942\u092A\
  \ \u0938\u0947 \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\u2026"
lastmod: '2024-04-05T21:53:54.671105-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u092E\u0947\u0902 \u0915\u094B\u0908 \u0928\u093F\u0930\u094D\
  \u092E\u093F\u0924 \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0922\u093E\u0902\
  \u091A\u093E \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\u0915\u093F\u0928\
  \ Pester, \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0924\u0943\
  \u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092E\u0949\u0921\u094D\u092F\u0942\
  \u0932, \u092A\u0930\u0940\u0915\u094D\u0937\u0923 \u0932\u093F\u0916\u0928\u0947\
  \ \u0914\u0930 \u091A\u0932\u093E\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0935\
  \u094D\u092F\u093E\u092A\u0915 \u0930\u0942\u092A \u0938\u0947 \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u0906\u092A\u0915\u0947 PowerShell\
  \ \u092B\u0902\u0915\u094D\u0936\u0928\u094D\u0938 \u0915\u093E \u092A\u0930\u0940\
  \u0915\u094D\u0937\u0923 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ Pester \u0915\u0947 \u0938\u093E\u0925 \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\
  \u0947 \u0915\u093E \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 \u0938\u092C\
  \u0938\u0947 \u092A\u0939\u0932\u0947, \u092F\u0926\u093F \u0906\u092A\u0928\u0947\
  \ \u092A\u0939\u0932\u0947 \u0938\u0947 Pester \u0907\u0902\u0938\u094D\u091F\u0949\
  \u0932 \u0928\u0939\u0940\u0902 \u0915\u093F\u092F\u093E \u0939\u0948 \u0924\u094B\
  \ \u0907\u0938\u0947 \u0907\u0902\u0938\u094D\u091F\u0949\u0932 \u0915\u0930\u0947\
  \u0902."
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
weight: 36
---

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
