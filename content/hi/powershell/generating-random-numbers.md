---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

अनुचित संख्याएँ उत्पन्न करना मतलब होता है कोई भी संख्या उत्पन्न करना जो मिलीजुली और अप्रत्याशित हो। प्रोग्रामर इसे करते हैं ताकि उनके कार्यक्रम तर्क, अनुमानित क्रियाएँ और प्रयोगात्मक परिणाम सिम्युलेट कर सकें।

## कैसे करें:

`Get-Random` PowerShell cmdlet उत्पन्न कर सकती है एक अनुचित संख्या।

```PowerShell
$randomNumber = Get-Random -Minimum 0 -Maximum 100
Write-Output $randomNumber
```

इसका आउटपुट हो सकता है कोई भी संख्या 0 से 100 के बीच।

## गहरा डाइव:

"Get-Random" cmdlet `.Net` framework के `System.Random` वर्ग का उपयोग करता है। यह एक सुदृढ़ अनुचित संख्या जनक है जो कि अन्य भाषाओं, जैसे C#, में भी उपलब्ध है।

विकल्पतः, "System.Security.Cryptography.RNGCryptoServiceProvider" वर्ग उपयोग कर सकते हैं अधिक गोपनीयता सुरक्षित करने के लिए, लेकिन इसका उपयोग आपसे अधिक सतर्कता की आवश्यकता होती है तथा इसमें प्रोग्रामिंग ज्ञान की जरूरत होती है।

## देखें भी:

- Microsoft's PowerShell "Get-Random" सहायता पृष्ठ: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1
- `.Net` मार्गदर्शिका पर `System.Random` वर्ग: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0
- `System.Security.Cryptography.RNGCryptoServiceProvider` वर्ग पर अधिक जानकारी: https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0