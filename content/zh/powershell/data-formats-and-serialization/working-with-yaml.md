---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:10.612930-07:00
description: "\u5982\u4F55\u64CD\u4F5C: PowerShell\u9ED8\u8BA4\u60C5\u51B5\u4E0B\u6CA1\
  \u6709\u5185\u7F6E\u7528\u4E8E\u89E3\u6790YAML\u7684cmdlet\uFF0C\u4F46\u662F\u5F53\
  \u4F60\u5229\u7528`powershell-yaml`\u6A21\u5757\u6216\u8005\u4F7F\u7528`ConvertFrom-Json`\u7ED3\
  \u5408\u50CF`yq`\u8FD9\u6837\u7684\u5DE5\u5177\u5C06YAML\u8F6C\u6362\u6210PowerShell\u5BF9\
  \u8C61\u65F6\uFF0C\u5B83\u80FD\u591F\u65E0\u7F1D\u5DE5\u4F5C\u3002 \u9996\u5148\uFF0C\
  \u5B89\u88C5\u6A21\u5757."
lastmod: '2024-03-13T22:44:48.037626-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u9ED8\u8BA4\u60C5\u51B5\u4E0B\u6CA1\u6709\u5185\u7F6E\u7528\u4E8E\
  \u89E3\u6790YAML\u7684cmdlet\uFF0C\u4F46\u662F\u5F53\u4F60\u5229\u7528`powershell-yaml`\u6A21\
  \u5757\u6216\u8005\u4F7F\u7528`ConvertFrom-Json`\u7ED3\u5408\u50CF`yq`\u8FD9\u6837\
  \u7684\u5DE5\u5177\u5C06YAML\u8F6C\u6362\u6210PowerShell\u5BF9\u8C61\u65F6\uFF0C\
  \u5B83\u80FD\u591F\u65E0\u7F1D\u5DE5\u4F5C."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作:
PowerShell默认情况下没有内置用于解析YAML的cmdlet，但是当你利用`powershell-yaml`模块或者使用`ConvertFrom-Json`结合像`yq`这样的工具将YAML转换成PowerShell对象时，它能够无缝工作。

### 使用 `powershell-yaml` 模块:
首先，安装模块:
```PowerShell
Install-Module -Name powershell-yaml
```

读取YAML文件:
```PowerShell
Import-Module powershell-yaml
$content = Get-Content -Path 'config.yml' -Raw
$yamlObject = ConvertFrom-Yaml -Yaml $content
Write-Output $yamlObject
```

将PowerShell对象写入YAML文件:
```PowerShell
$myObject = @{
    name = "John Doe"
    age = 30
    languages = @("PowerShell", "Python")
}
$yamlContent = ConvertTo-Yaml -Data $myObject
$yamlContent | Out-File -FilePath 'output.yml'
```

`output.yml` 示例:
```yaml
name: John Doe
age: 30
languages:
- PowerShell
- Python
```

### 使用 `yq` 和 `ConvertFrom-Json` 解析YAML:
另一种方法涉及使用`yq`，一个轻量级且可移植的命令行YAML处理器。`yq`可以将YAML转换成JSON，而PowerShell可以本地解析JSON。

首先，确保系统上已安装`yq`。
然后运行:
```PowerShell
$yamlToJson = yq e -o=json ./config.yml
$jsonObject = $yamlToJson | ConvertFrom-Json
Write-Output $jsonObject
```

这种方法特别适用于在跨平台环境中工作或者偏好在PowerShell中使用JSON的用户。
