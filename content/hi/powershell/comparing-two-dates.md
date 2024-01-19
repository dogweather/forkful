---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

दो तारीखों की तुलना एक ऐसी प्रक्रिया है जिसमें हम एक तारीख को दूसरी तारीख से मुकाबला करते हैं। प्रोग्रामर्स इसे तारीखों के आधार पर डेटा सॉर्ट और फ़िल्टर करने के लिए करते हैं।

## कैसे:

पॉवरशेल में दो तारीखों की तुलना क्या दिखती है, वह यहां देखिए:

```PowerShell
$date1 = Get-Date -Year 2022 -Month 1 -Day 1
$date2 = Get-Date -Year 2022 -Month 1 -Day 2
if ($date1 -gt $date2) { 
    "Date1 is later than Date2"
} elseif ($date1 -eq $date2) {
    "Date1 is the same as Date2"
} else {
    "Date1 is earlier than Date2"
}
```

आउटपुट:

```PowerShell
"Date1 is earlier than Date2"
```

## गहरी जांच

1. **ऐतिहासिक प्रसंग**: पॉवरशेल में तारीखों की तुलना करने की क्षमता, उसके कोर .NET फ्रेमवर्क से आती है, जो 2000 के दशक से उपलब्ध है।
2. **विकल्प**: दूसरे स्क्रिप्टिंग भाषाओं, जैसे कि पायथन और जावास्क्रिप्ट, भी तारीखों की तुलना की क्षमता प्रदान करते हैं।
3. **विवरण**: पॉवरशेल में तारीखों की तुलना, `-gt`, `-lt`, `-eq` तुलना ऑपरेटर का उपयोग करके की जाती है।

## भी देखें:

- [पॉवरशेल डाक्युमेंटेशन: तारीखों की तुलना](https://docs.microsoft.com/powershell/)
- [Powershell: Working with Dates](https://www.computerperformance.co.uk/powershell/powershell-dates/)
- [StackOverflow: Comparing Two Dates in Powershell](https://stackoverflow.com/questions/7835422/comparing-two-dates-in-powershell)