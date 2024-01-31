---
title:                "Working with JSON"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) is a lightweight data format easy for humans to read and write, easy for machines to parse and generate. Programmers work with JSON to exchange data between web clients and servers or to store data because it's simple and has become a web standard.

## How to: 
### Reading JSON
```PowerShell
# Assume 'data.json' contains {"name": "John", "age": 30}
$json = Get-Content -Path 'data.json' | ConvertFrom-Json
# Output the name
$json.name  # Outputs: John
```

### Writing JSON
```PowerShell
$person = @{name='Jane'; age=25}
$person | ConvertTo-Json | Set-Content -Path 'person.json'
# person.json now contains: 
# {
#     "age":  25,
#     "name":  "Jane"
# }
```

### Modifying JSON
```PowerShell
$json = Get-Content -Path 'person.json' | ConvertFrom-Json
$json.age = 26
$json | ConvertTo-Json | Set-Content -Path 'person.json'
# person.json now updates Jane's age to 26
```

## Deep Dive
JSON's been the go-to for web data since the early 2000s, taking the throne from XML because of its simplicity. Alternatives to JSON include YAML and the newer TOML, but JSON reigns due to its widespread support and alignment with JavaScript's object syntax. When working with JSON in PowerShell, the built-in `ConvertFrom-Json` and `ConvertTo-Json` cmdlets are powerful, but keep an eye on their depth limits and the `[PSCustomObject]` PowerShell type used when converting from JSON.

## See Also
- [JSON.org](https://www.json.org/json-en.html) for JSON's syntax and basics
- [MDN Web Docs on JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON) for the JavaScript side
