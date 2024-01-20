---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

सेल-आर्डर आर्ग्यूमेंट पढ़ना, एक प्रोग्राम के उन्नयन के दौरान उपयोगकर्ता की इनपुट से निपटने का एक तरीका है। प्रोग्रामर्स इसे उपयोगकर्ता विशिष्ट विवरण और मानगणना प्रदान करने के लिए करते हैं, जो की स्क्रिप्ट के चलने के तरीके को बदल सकती हैं।

## कैसे:

```PowerShell
param (
    [string]$Argument1,
    [string]$Argument2
)

echo "Argument 1 is $Argument1"
echo "Argument 2 is $Argument2"
```
इसे निम्नलिखित आउटपुट मिलेगा:

```PowerShell
> .\MyScript.ps1 -Argument1 "Hello" -Argument2 "World"
Argument 1 is Hello
Argument 2 is World
```

## गहरी डुबकी:

PowerShell में कमांड लाइन आर्ग्यूमेंट पढ़ने संबंधी समाधान का इतिहास Unix शेल स्क्रिप्टिंग से समानताएं रखता है। वैकल्पिक रूप से, आप `$args` वेरिएबल का उपयोग कर सकते हैं जो एक ऐरे होता है जिसमें सभी नियुक्त इनपुट संग्रहीत होते हैं।
  
```PowerShell
# Using $args
echo "First arg:  $args[0]"
echo "Second arg: $args[1]"
```

यह कोई $\color{green}{arguments}$ स्वीकार कर सकता है, न की केवल दो। ध्यान दें: `$args` का उपयोग खुद आपके कोड को अधिक "सामान्य" बनाने के लिए है।

## अधिक देखें:

[`About Parameters in PowerShell`](https://docs.microsoft.com/powershell/scripting/lang-spec/variables?view=powershell-7.2#the-args-variable)
[`Using "$args"`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_parameters?view=powershell-7.1#using-args)
[`About Functions`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions?view=powershell-7.1)