---
title:                "दो तारीखों की तुलना करना।"
html_title:           "PowerShell: दो तारीखों की तुलना करना।"
simple_title:         "दो तारीखों की तुलना करना।"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
दो तारीखों को तुलना करना, यह है कि दो अलग-अलग दिनांकों को कैसे तुलना किया जा सकता है और क्यों कोडर यह करते हैं।

## कैसे:
```PowerShell
$date1 = Get-Date "01/01/2020"
$date2 = Get-Date "01/01/2021"

# दो तारीखों को तुलना करने के लिए, हम Compare-Object cmdlet का प्रयोग कर सकते हैं
Compare-Object -ReferenceObject $date1 -DifferenceObject $date2
```

भागवान ने दोनों तारीखों को तुलना किया कि एक नया तिथि ग्रेगोरियन कैलेंडर में है।

```PowerShell
# इनपुट पैरामीटर का संसाधन तय करें और तुलना शून्य का प्रयोग करें
Compare-Object -ReferenceObject $date1 -DifferenceObject (Get-Date "01/01/2021") -Tolerance 0
```
इस कमांड के लिए तुलना ० है, जो तुलना के द्वारा दस्तावेजीकृत तारीखों इकाई है।

## गहन अध्ययन:
तारीखों को तुलना करने के पीछे हाईस्टोरिकल कॉन्क्टेक्स्ट, अल्टरनेटिव्स, और इम्पिलेमेंशन डिटेल्स है। यह महत्वपूर्ण है कि दो तारीखों को अंतर को भाग दिया जाता है या कोई अंतर अलग तारीखों में है।

## इससे जुड़े और अधिक जानकारी के लिए:
ReferenceObject का उपयोग करना हमें दो तारीखों को तुलना करने की अन्वेशना करेगा हम यह भी देख सकते हैं कि मानविकों में अंतर है, इससे जुड़े साथ-साथ और अधिक जानकारी के लिए Microsoft के <a href="https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/compare-object">Compare-Object documentation</a> का उपयोग करें।