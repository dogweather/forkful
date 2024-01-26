---
title:                "Working with YAML"
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML is a human-friendly data serialization format. Programmers use it for configuration files, data exchange between languages, and because it’s easy to read and write compared to XML or JSON.

## How to:
To work with YAML in PowerShell, you'll need to use a module like `powershell-yaml`. Install it first:

```PowerShell
Install-Module -Name powershell-yaml
```

Reading YAML content:

```PowerShell
# Import the module
Import-Module powershell-yaml

# Load a YAML file
$yamlContent = Get-Content -Path 'config.yaml' -Raw

# Convert YAML to a PowerShell object
$configObject = ConvertFrom-Yaml -Yaml $yamlContent

# Output the object
$configObject
```

Creating and writing YAML:

```PowerShell
# Create a hashtable
$person = @{
  name = 'Jane Doe'
  age = 30
  languages = @('English', 'French')
}

# Convert the hashtable to YAML
$yamlOutput = ConvertTo-Yaml -Data $person

# Write the YAML to a file
$yamlOutput | Out-File -FilePath 'person.yaml'
```

## Deep Dive
YAML originated in the early 2000s and stands for “YAML Ain't Markup Language,” a recursive acronym highlighting its data-centric approach over markup languages like HTML. While JSON is often the go-to for APIs and web services due to its efficient parsing and compactness, YAML remains popular for its readability and being more human-editable, especially in configuration files (e.g., Docker Compose and Kubernetes).

Alternatives to `powershell-yaml` include `YamlDotNet` with a `.NET` glue code, or manually parsing YAML strings - but why complicate your life?

Under the hood, `powershell-yaml` uses `YamlDotNet`, converting YAML to .NET objects which PowerShell can easily handle. This interoperation allows smooth transitioning of YAML data into the PowerShell ecosystem.

## See Also
- [`powershell-yaml` on PowerShell Gallery](https://www.powershellgallery.com/packages/powershell-yaml)
- [YAML Official Website](https://yaml.org/)
- [YAML Syntax Reference](https://learnxinyminutes.com/docs/yaml/)
