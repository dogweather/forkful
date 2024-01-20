---
title:                "JSON के साथ काम करना"
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON kaam kya hai, aur programmers iska istemal kyun karte hain? JSON yani JavaScript Object Notation, ek data format hai jo settings, configuration, aur data exchange ke liye istemal hota hai. Programmers isko isliye pasand karte hain kyunki ye padhne mein aasaan aur machine ke liye interpret karne mein efficient hota hai.

## How to:
PowerShell mein JSON se kaam karne ke kuch basic examples yeh hain:

Convert from JSON:
```PowerShell
$jsonData = '{"name": "Amit", "age": 30, "city": "Delhi"}'
$person = $jsonData | ConvertFrom-Json
$person.name
$person.age
$person.city
```
Output: Amit, 30, Delhi

Convert to JSON:
```PowerShell
$personObject = [PSCustomObject][Ordered]@{
    name = "Amit"
    age = 30
    city = "Delhi"
}
$json = $personObject | ConvertTo-Json
Write-Output $json
```
Output: 
```json
{
    "name": "Amit",
    "age": 30,
    "city": "Delhi"
}
```

## Deep Dive:
JSON ki shuruaat Douglas Crockford ne ki thi, jo JavaScript ke syntax par based hai. Alternatives mein XML aur YAML aate hain, lekin JSON unse zyada compact aur fast hota hai. PowerShell mein JSON ko handle karne ke liye mainly `ConvertFrom-Json` aur `ConvertTo-Json` cmdlets ka istemal hota hai. Complex objects ko handle karte time, JSON ko sahi se format karne aur nested objects se deal karne ke liye aapko PowerShell mein thodi aur advanced coding karna pad sakta hai.

## See Also:
JSON ke baare mein aur jaankari ke liye, yeh resources helpful honge:

- [JSON schema store](https://www.schemastore.org/json/)