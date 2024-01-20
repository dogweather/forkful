---
title:                "भविष्य या भूतकाल में तारीख की गणना"
html_title:           "PowerShell: भविष्य या भूतकाल में तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में तारीख की गणना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या अतीत में तारीख की गणना वो प्रक्रिया है जिसके माध्यम से हम अगली या पिछली तारीख जान सकते हैं। यह बहुत अक्सर वेब विकास, प्रोग्रामिंग और डेटा एनालिटिक्स में कार्य करते समय होती है।

## कैसे करें:

हम PowerShell का उपयोग करके भविष्य या अतीत की तारीख की गणना कर सकते हैं। यहां पर कैसे किया जा सकता है:

```PowerShell
# भविष्य में तारीख की गणना करने के लिए
$futureDate = (Get-Date).AddDays(5)
Write-Output $futureDate
```

आउटपुट:

```PowerShell
Monday, February 6, 2023 12:05:28 PM
```

```PowerShell
# अतीत में तारीख की गणना करने के लिए
$pastDate = (Get-Date).AddDays(-5)
Write-Output $pastDate
```

आउटपुट:

```PowerShell
Wednesday, February 1, 2023 12:05:28 PM
```

## गहरी जाँच:

1. ऐतिहासिक संदर्भ: PowerShell की पीढ़ी 1.0 से ही तारीख और समय के साथ काम करने के लिए cmdlets मौजूद थे।

2. विकल्प: अन्य भाषाओं में, जैसे कि Python या JavaScript, इसे भिन्न ढंग से किया जाता है।

3. कार्यान्वयन विवरण: `(Get-Date).AddDays()` एक तारीख वापस देता है, जो वर्तमान तारीkh से अधिक या कम होती है, आवश्यकतानुसार।

## अधिक देखें:

1. [PowerShell Get-Date cmdlet Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)