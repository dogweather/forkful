---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेगुलर एक्सप्रेशन्स (regular expressions) पैटर्न मैचिंग टूल्स हैं, जो टेक्स्ट में विशिष्ट डेटा ढूँढने या मैनिपुलेट करने के लिए इस्तेमाल होते हैं. प्रोग्रामर्स इनका इस्तेमाल इसलिए करते हैं, क्योंकि ये कम्पलेक्स सर्च और रिप्लेसमेंट टास्क्स को आसान और एफिसिएंट बनाते हैं.

## How to: (कैसे करें)
PowerShell में रेगुलर एक्सप्रेशन्स का इस्तेमाल करके टेक्स्ट मैच और रिप्लेस करने के उदाहरण:

```PowerShell
# मैचिंग ईमेल एड्रेस
$emails = "test@example.com", "hello@world.net", "noemailatall"
$pattern = '^\S+@\S+\.\S+$'
$emails -match $pattern

# स्ट्रिंग की शुरुआत में 'h' वाले शब्द
$words = "hello", "hey", "goodbye", "hi"
$words -match '^h'

# फाइल्स की लिस्ट से .txt एक्सटेंशन वाली फाइल्स खोजना
$files = Get-ChildItem
$txtFiles = $files -match '\.txt$'
$txtFiles
```

सैंपल आउटपुट:
```
true
true
false

hello
hey

file1.txt
notes.txt
```

## Deep Dive (गहराई में जानकारी)
रेगुलर एक्सप्रेशन्स की शुरुआत 1950s में हुई थी. ये अनेकों प्रोग्रामिंग भाषाओं में इम्प्लीमेंट किए गए हैं. PowerShell में, ये .NET के System.Text.RegularExpressions नेमस्पेस का इस्तेमाल करते हैं.

अल्टरनेटिव्स में 'like' ऑपरेटर या वाइल्डकार्ड पैटर्न्स शामिल हैं, लेकिन ये रेगुलर एक्सप्रेशन्स की तरह शक्तिशाली नहीं होते. 

परफॉरमेंस के लिहाज से, पैटर्न्स को सिंपल रखने और बैकट्रैकिंग कम से कम करने पर ध्यान देना चाहिए.

## See Also (और भी देखें)
- Microsoft की आधिकारिक PowerShell डॉक्युमेंटेशन साईट पर रेगुलर एक्सप्रेशन्स का विस्तृत अध्यायन: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
- .NET रेगुलर एक्सप्रेशन्स के लिए संदर्भ गाईड: [System.Text.RegularExpressions Namespace](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions?view=netframework-4.8)
- रेगुलर एक्सप्रेशन टेस्टिंग और एक्सपेरिमेंटेशन के लिए ऑनलाइन टूल: [Regex101](https://regex101.com/)
