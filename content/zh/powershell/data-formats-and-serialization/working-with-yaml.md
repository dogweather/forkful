---
title:                "使用YAML工作"
aliases:
- /zh/powershell/working-with-yaml.md
date:                  2024-02-03T19:26:10.612930-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么与为什么?
YAML，全称为 YAML Ain't Markup Language（YAML不是标记语言），是一种人类可读的数据序列化语言。程序员通常使用它来处理配置文件以及实现不同编程语言之间的数据传输。它的简洁性和可读性使其特别适用于需要设置环境、应用程序或服务的任务，这些配置必须是关键的、易于理解和编辑的。

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
