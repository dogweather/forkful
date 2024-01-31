---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-27T20:35:21.426630-07:00
model:                 gpt-4-0125-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"

category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
PowerShell में यादृच्छिक संख्याएँ उत्पन्न करने का तात्पर्य एक निश्चित श्रेणी के भीतर अनुमानित नहीं किए जा सकने वाले संख्या मूल्यों को बनाने से है। प्रोग्रामर इस क्षमता का उपयोग अनेक कारणों से करते हैं, जिनमें परीक्षण, सिमुलेशन और सुरक्षा उद्देश्य शामिल हैं, जहाँ अनिश्चितता या वास्तविक दुनिया की यादृच्छिकता की नक़ल करना महत्वपूर्ण है।

## कैसे करें:
PowerShell, `Get-Random` cmdlet का उपयोग करके यादृच्छिक संख्याएँ उत्पन्न करने का एक सरल दृष्टिकोण प्रदान करता है। यह cmdlet एक डिफ़ॉल्ट श्रेणी या एक निर्दिष्ट श्रेणी के भीतर यादृच्छिक संख्याएँ उत्पन्न कर सकता है।

```PowerShell
# 0 और Int32.MaxValue के बीच एक यादृच्छिक संख्या उत्पन्न करें
$randomNumber = Get-Random
Write-Output $randomNumber
```

एक श्रेणी निर्दिष्ट करने के लिए, `-Minimum` और `-Maximum` पैरामीटर का उपयोग करें:

```PowerShell
# 1 और 100 के बीच एक यादृच्छिक संख्या उत्पन्न करें
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

अधिक नियंत्रण के लिए, आप `System.Random` क्लास का एक ऑब्जेक्ट इंस्टेंट कर सकते हैं:

```PowerShell
# संख्याओं के एक क्रम के लिए System.Random का उपयोग करना
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

यदि आपको एक ऐरे या संग्रह से एक यादृच्छिक चयन की आवश्यकता है, तो `Get-Random` सीधे एक आइटम चुन सकता है:

```PowerShell
# एक ऐरे से रैंडम चयन
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## गहराई में जाने पर
PowerShell में `Get-Random` cmdlet, .NET क्लास `System.Random` का उपयोग करके यादृच्छिक संख्या उत्पन्न करता है। ये "प्रतीक" होते हैं क्योंकि वे एल्गोरिदम का उपयोग करके संख्याओं की शृंखला उत्पन्न करते हैं जो केवल यादृच्छिक प्रतीत होती हैं। अधिकतर अनुप्रयोगों के लिए, यह यादृच्छिकता का स्तर पर्याप्त है। हालांकि, क्रिप्टोग्राफिक सुरक्षा की आवश्यकता वाले मामलों में, `System.Random` इसकी पूर्वानुमेय प्रकृति के कारण उपयुक्त नहीं है।

क्रिप्टोग्राफिक यादृच्छिकता के लिए, PowerShell और .NET `System.Security.Cryptography.RNGCryptoServiceProvider` प्रदान करते हैं, जो एन्क्रिप्शन कुंजी या अन्य सुरक्षा-संवेदनशील ऑपरेशनों को उत्पन्न करने के लिए अधिक उपयुक्त है:

```PowerShell
# क्रिप्टोग्राफिकली सुरक्षित यादृच्छिक संख्याएँ
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

जबकि `Get-Random` और `System.Random` स्क्रिप्टिंग और अनुप्रयोग तर्क के लिए यादृच्छिकता की एक विस्तृत श्रेणी की आवश्यकताओं को संतुष्ट करते हैं, यह महत्वपूर्ण है कि जब सुरक्षा-केंद्रित अनुप्रयोगों में पूर्वानुमेयता एक कमजोरी प्रस्तुत कर सकती है, तो नौकरी के लिए सही उपकरण का चयन करें।
