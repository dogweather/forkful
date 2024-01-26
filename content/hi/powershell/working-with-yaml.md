---
title:                "यामल के साथ काम करना"
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML ("YAML Ain't Markup Language") एक data serialization format है जो आसानी से पढ़े जाने वाले data representation के लिए इस्तेमाल होता है। Programmers इसे configurations, data exchange और storage settings में इस्तेमाल करते हैं क्योंकि यह human-readable होता है और JSON, XML की तुलना में सरल होता है।

## How to: (कैसे करें:)
```PowerShell
# YAML file को पढ़ना
$yamlContent = Get-Content -Path 'example.yml' -Raw
$parsedYaml = ConvertFrom-Yaml $yamlContent
Write-Host "YAML content as a PS Object:"
$parsedYaml

# PowerShell object को YAML में बदलना
$psObject = @{
  name = 'PowerShell'
  version = 'current'
}
$yamlFormat = ConvertTo-Yaml $psObject
$yamlFormat | Out-File -FilePath 'output.yml'
```

Sample Output:
```
YAML content as a PS Object:
name: PowerShell
version: current
```

## Deep Dive (गहराई से जानकारी):
YAML 2001 में आया और इसे तब से configurations और data storage के लिए बहुत ज्यादा अपनाया गया है। JSON और XML इसके alternatives हैं, लेकिन YAML है human-readable और less verbose, इसलिए उपयोग में आरामदायक है। PowerShell में yaml पढ़ने या लिखने के लिए `yaml` मॉड्यूल का इस्तेमाल होता है, जिसे अलग से install करना पड़ सकता है।

## See Also (और भी जानकारी के लिए):
- YAML Syntax: https://yaml.org/spec/1.2/spec.html
- PowerShell Gallery YAML module: https://www.powershellgallery.com/packages/YamlDotNet
- JSON vs YAML: https://blog.yaml.io/understanding-json-vs-yaml/
