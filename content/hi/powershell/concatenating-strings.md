---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

Vastuk (Strings) ko jodne ko hum 'Concatenating Strings' kehte hain. Yah vastukon (strings) ko ek saath jodne ka tareeka hai jo programmer ko kisi ek saath anek sootra (strings) ko prastut karne mein madad karta hai.

## कैसे:

पावरशेल में, आप '+' या '-f' उपयोग करके वस्तुओं (strings) को जोड़ सकते हैं। यहां कुछ उदाहरण दिए गए हैं :

```PowerShell
$name = "World"
$message = "Hello, " + $name
$message
```
आउटपुट:
```
Hello, World
```
या फिर, आप '-f' ऑपरेटर का उपयोग कर सकते हैं:
```PowerShell
$name = "World"
$message = "Hello, {0}" -f $name
$message
```
आउटपुट:
```
Hello, World
```
## गहरा अवलोकन:

Vastuk (Strings) को jodne (concatenating) ka upyog 1970 k decade se kiya jaa raha hai. Python aur JavaScript jaise aur programming languages mein bhi isi tarah ka upyog hota hai.

वैकल्पिक तरीके में, आप concatenate के बजाय PowerShell में `-join` ऑपरेटर का उपयोग कर सकते हैं:

```PowerShell
$strings = "Hello", "World"
$message = $strings -join ', '
$message
```
आउटपुट:
```
Hello, World
```
यह तरीका आपको एक array की सभी वस्तुओं (वस्त्रों) को एक साथ जोड़ने की अनुमति देता है, जिससे कोड का स्वच्छता और पढ़ने में आसानी होती है।

## अधिक जानकारी के लिए:

- [Concatenation Operators in PowerShell](https://www.powershellmagazine.com/2012/07/23/pstip-concatenation-operators-in-powershell/)
- [About Join Operator](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_join?view=powershell-7.1)