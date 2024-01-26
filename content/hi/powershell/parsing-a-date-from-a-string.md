---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:37:55.851272-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख को स्ट्रिंग से पार्स करने का मतलब है, टेक्स्ट फॉर्मैट में दी गई तारीख को कंप्यूटर की भाषा में तब्दील करना ताकि सिस्टम इसे समझ और प्रोसेस कर सके। प्रोग्रामर्स इसे इसलिए करते हैं ताकि तारीख से संबंधित अलग-अलग ऑपरेशन्स को आसानी से परफॉर्म किया जा सके।

## How to: (कैसे करें:)
```PowerShell
# स्ट्रिंग से डेट पार्स करने का एक उदाहरण
$dateString = "31-01-2023"
$parsedDate = [datetime]::ParseExact($dateString, 'dd-MM-yyyy', $null)

# पार्स की गई तारीख दिखाएं
$parsedDate
```
सैंपल आउटपुट:
```
Tuesday, January 31, 2023 12:00:00 AM
```

## Deep Dive (गहराई से जानकारी)
पावरशेल में तारीख को पार्स करना काफ़ी आम है। `[datetime]::ParseExact` मेथड .NET फ्रेमवर्क से आता है और यह तारीख और समय को सटीक प्रारूप में पार्स करने के लिए इस्तेमाल होता है। पावरशेल 7 अपने पिछले संस्करणों के मुकाबले अधिक क्रॉस-प्लेटफ़ॉर्म और आधुनिक .NET कोर फ्रेमवर्क पर बना है।

कई तरीके हैं जिनके द्वारा डेट को पार्स किया जा सकता है; `ParseExact`, `TryParse`, `TryParseExact` उदाहरण हैं। `TryParse` और `TryParseExact` इस्तेमाल करने पर अगर पार्सिंग में कोई त्रुटि आती है, तो वे फॉल्स (false) रिटर्न करते हैं और एक्सेप्शन (exception) थ्रो नहीं करते, जिससे एरर हैंडलिंग आसान हो जाती है।

इतिहास के संदर्भ में, पावरशेल ने हमेशा .NET फ्रेमवर्क के मजबूत दिनांक और समय फ़ंक्शन्स का फायदा उठाया है। पावरशेल स्क्रिप्टिंग के दौरान डेट पार्सिंग बहुत ही स्थानिक कामों का हिस्सा है, चाहे वो फाइल्स के टाइमस्टैम्प्स वेरिफाई करना हो या लॉग्स में से डेटा एक्सट्रेक्ट करना।

## See Also (और जानकारी के लिए)
- [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- [.NET DateTime.ParseExact Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=netcore-3.1)
- [PowerShell GitHub repository](https://github.com/PowerShell/PowerShell)
