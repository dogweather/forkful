---
date: 2024-01-25 03:39:51.843009-07:00
description: "How to: In PowerShell, there's no native cmdlet to parse TOML. You'd\
  \ typically use a module or convert TOML to JSON with a tool like `toml-to-json`\
  \ if you\u2026"
lastmod: '2024-03-13T22:45:00.305393-06:00'
model: gpt-4-1106-preview
summary: In PowerShell, there's no native cmdlet to parse TOML.
title: Working with TOML
weight: 39
---

## How to:
In PowerShell, there's no native cmdlet to parse TOML. You'd typically use a module or convert TOML to JSON with a tool like `toml-to-json` if you want to work with PowerShell. Here's how you'd do it with a fictitious module `PowerShellTOML`:

```PowerShell
# First, install the module (imaginary, for demonstration)
Install-Module PowerShellTOML

# Import a TOML file
$config = Import-TomlConfig -Path './config.toml'

# Accessing a value
Write-Output $config.database.server

# Sample TOML content in 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Sample output:
# 192.168.1.1
```

## Deep Dive
TOML was created by Tom Preston-Werner, co-founder of GitHub, as a simpler alternative to XML and YAML for configuration files. Its first version appeared in 2013. TOML is comparable to JSON but is designed to be more human-friendly, making it a good choice for configuration that's maintained by people. Alternatives include YAML, JSON, and XML.

In terms of implementation, a PowerShell module for TOML would typically be a wrapper around a TOML library written in a more performance-oriented language like C#. PowerShell doesn't have built-in support for TOML, which is why such a module is necessary to interface with the TOML format conveniently.

## See Also
- TOML standard: https://toml.io/en/
- GitHub repository for `toml` PowerShell module (if exists at the time of reading): https://github.com/powershell/PowerShellTOML
- An introduction to TOML: https://github.com/toml-lang/toml
- Comparison of data serialization formats: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
