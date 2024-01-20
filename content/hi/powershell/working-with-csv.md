---
title:                "कंप्यूटर प्रोग्रामिंग में csv का साथ लेना"
html_title:           "PowerShell: कंप्यूटर प्रोग्रामिंग में csv का साथ लेना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में csv का साथ लेना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV फ़ाइल फार्मेट text files को संगठित करने के लिए इस्तेमाल किया जाता है जो दो से अधिक columns और rows से बनता है। ये एक spreadsheet जैसा है जो चिह्न अलग करने के लिए commas और कोटेशन मार्क का उपयोग करता है। प्रोग्रामर्स CSV फ़ाइल को इस्तेमाल क्यों करते हैं क्योंकि ये आसानी से पढ़ा जा सकता है और डेटा को संरचित और अनुकूलित करने के लिए फ़ाइल में कई row और column हो सकते हैं।

## कैसे करें:

```
# CSV फ़ाइल पढ़ना
$csvData = Import-CSV 'C:\data.csv'

# डेटा को फ़ाइल में लिखना
$data | Export-CSV 'C:\newdata.csv'

# डेटा सॉर्ट करना और नई फ़ाइल में लिखना
$data | Sort-Object Name | Export-CSV 'C:\sorteddata.csv'

# CSV फ़ाइल से डेटा को filter करना
$filteredData = $csvData | Where-Object {$_.Age -gt 30} | Export-CSV 'C:\filtereddata.csv'
```

## गहराई पानी:

आप Microsoft Excel या अन्य spreadsheet software का उपयोग करके भी CSV फ़ाइलों को खोल सकते हो। हालांकि, PowerShell आपको कोई भी डेटा की मुल्यांकन या संशोधन से पहले CSV फ़ाइल को संपादित करने की अनुमति देता है। CSV फ़ाइल की पाँच टोलों से जुड़ी जानकारी के आधार पर फ़ाइल को सही तरीके से समायोजित किया जा सकता है:

1. C - कॉमा: ये चिह्न columns को अलग करने के लिए कम मान हैं।
2. R - रिटर्न: ये चिह्न rows को अलग करने के लिए कम मान हैं।
3. V - वीरा: ये चिह्न values को quotes में रखने के लिए होते हैं।
4. T - टैब: ये चिह्न tab characters को columns को अलग करने के लिए कम मान हैं।
5. S - स्पेस: ये चिह्न columns और rows को spaces से अलग करने के लिए होते हैं।

PowerShell के अतिरिक्त, आप CSV फ़ाइल की format से उन चिह्नों को हटा सकते हैं जो उपयोग में नहीं होते हैं, जैसे कि quotes और tabs।

## देखें भी:

- [Understanding CSV File Format](https://www.youtube.com/watch?v=VXLbJScZGpI)
- [Working with CSV Files in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv?view=powershell-7)