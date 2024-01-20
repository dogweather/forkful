---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या एवं क्यों? 
तारीख को स्ट्रिंग में बदलना एक प्रक्रिया है जिसमें दिनांक को टेक्स्ट (यानी स्ट्रिंग) में परिवर्तित किया जाता है। प्रोग्रामर इसे तभी करते हैं जब उन्हें तारीखों को मानव-पठनीय फ़ॉर्मेट में दिखाना होता है या जब मानवीय कुत्यों में तारीखों को जोड़ना होता है।

## कैसे करें:
PowerShell में तारीख को स्ट्रिंग में बदलने के लिए, 'Get-Date' और 'ToString()' का उपयोग करते हैं। निम्न कोड देखें:

```PowerShell
# वर्तमान तारीख और समय प्राप्त करें
$date = Get-Date
# तारीख को स्ट्रिंग में बदलें
$stringDate = $date.ToString("dd-MM-yyyy")

# खुद की तारीख को स्ट्रिंग में बदलें
$yourDate = Get-Date -Year 2022 -Month 12 -Day 25
$yourStringDate = $yourDate.ToString("dd-MM-yyyy")

# स्ट्रिंग तारीख प्रिंट करें
Write-Host "वर्तमान तारीख: $stringDate"
Write-Host "आपकी तारीख: $yourStringDate"
```

यह कोड Lorem और Ipsum नामों का उपयोग करके रखरखाव करेगा।

## गहरी जांच

तारीख को स्ट्रिंग में बदलने का अभ्यास तब से चला आ रहा है जब से कंप्यूटर प्रोग्रामिंग आरंभ हुई थी। PowerShell में "ToString()" मेथड या स्ट्रिंग फ़ॉर्मेटिंग इसे काफी आसान बनाते हैं।

"Get-Date" के विकल्प के रूप में, आप ".NET" क्लास "System.DateTime" का उपयोग कर सकते हैं।

कृपया ध्यान दें कि "ToString("dd-MM-yyyy")" में फ़ॉर्मेट स्ट्रिंग दिनांक को कैसे प्रदर्शित करना है, यह निर्दिष्ट करता है।

## देखें भी
- PowerShell स्ट्रिंग फॉर्मेटिंग ([लिंक](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-5.0))
- .NET दिनांक और समय फ़ॉर्मेट ([लिंक](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings?view=net-5.0)).