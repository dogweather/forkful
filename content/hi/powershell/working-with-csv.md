---
title:                "CSV के साथ काम करना"
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV, यानी Comma-Separated Values, एक सिम्पल फ़ाइल फ़ॉर्मेट है जो टेबुलर डेटा को स्टोर करता है। प्रोग्रामर्स इसका इस्तेमाल करते हैं क्योंकि यह स्प्रेडशीट्स और डेटाबेस के बीच डेटा का आसान आदान-प्रदान करने का एक सहज तरीका है।

## How to: (कैसे करें:)
### CSV फ़ाइल इम्पोर्ट करना
```PowerShell
$csvData = Import-Csv -Path "path/to/your/file.csv"
$csvData
```
### CSV फ़ाइल एक्सपोर्ट करना
```PowerShell
$object | Export-Csv -Path "path/to/your/newfile.csv" -NoTypeInformation
```
### CSV डेटा को कस्टम ऑब्जेक्ट्स में बदलना
```PowerShell
$csvData | ForEach-Object {
    [PSCustomObject]@{
        Name = $_.Name
        Email = $_.Email
        Age = $_.Age
    }
}
```

## Deep Dive (गहराई से समझिए)
CSV फॉर्मेट 1970s से इस्तेमाल हो रहा है। Alternative फॉर्मेट्स में JSON और XML हैं, लेकिन CSV का सादगी में मज़े की बात होती है। PowerShell में CSV के साथ काम करने के लिए `Import-Csv` और `Export-Csv` जैसे cmdlets होते हैं, जो डेटा को आसानी से पर्स और आउटपुट कर सकते हैं।

## See Also (और जानकारी)
- [PowerShell Documentation on Microsoft Docs](https://docs.microsoft.com/powershell/)
- [About Csv on PowerShell | Microsoft Docs](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/import-csv?view=powershell-7.1)