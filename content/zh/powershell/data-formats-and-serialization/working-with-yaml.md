---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:10.612930-07:00
description: "YAML\uFF0C\u5168\u79F0\u4E3A YAML Ain't Markup Language\uFF08YAML\u4E0D\
  \u662F\u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\
  \u7684\u6570\u636E\u5E8F\u5217\u5316\u8BED\u8A00\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\
  \u4F7F\u7528\u5B83\u6765\u5904\u7406\u914D\u7F6E\u6587\u4EF6\u4EE5\u53CA\u5B9E\u73B0\
  \u4E0D\u540C\u7F16\u7A0B\u8BED\u8A00\u4E4B\u95F4\u7684\u6570\u636E\u4F20\u8F93\u3002\
  \u5B83\u7684\u7B80\u6D01\u6027\u548C\u53EF\u8BFB\u6027\u4F7F\u5176\u7279\u522B\u9002\
  \u7528\u4E8E\u9700\u8981\u8BBE\u7F6E\u73AF\u5883\u3001\u5E94\u7528\u7A0B\u5E8F\u6216\
  \u670D\u52A1\u7684\u4EFB\u52A1\uFF0C\u8FD9\u4E9B\u914D\u7F6E\u5FC5\u987B\u662F\u5173\
  \u952E\u7684\u3001\u6613\u4E8E\u7406\u89E3\u548C\u7F16\u8F91\u7684\u3002"
lastmod: '2024-03-13T22:44:48.037626-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u5168\u79F0\u4E3A YAML Ain't Markup Language\uFF08YAML\u4E0D\u662F\
  \u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\u7684\
  \u6570\u636E\u5E8F\u5217\u5316\u8BED\u8A00\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u4F7F\
  \u7528\u5B83\u6765\u5904\u7406\u914D\u7F6E\u6587\u4EF6\u4EE5\u53CA\u5B9E\u73B0\u4E0D\
  \u540C\u7F16\u7A0B\u8BED\u8A00\u4E4B\u95F4\u7684\u6570\u636E\u4F20\u8F93\u3002\u5B83\
  \u7684\u7B80\u6D01\u6027\u548C\u53EF\u8BFB\u6027\u4F7F\u5176\u7279\u522B\u9002\u7528\
  \u4E8E\u9700\u8981\u8BBE\u7F6E\u73AF\u5883\u3001\u5E94\u7528\u7A0B\u5E8F\u6216\u670D\
  \u52A1\u7684\u4EFB\u52A1\uFF0C\u8FD9\u4E9B\u914D\u7F6E\u5FC5\u987B\u662F\u5173\u952E\
  \u7684\u3001\u6613\u4E8E\u7406\u89E3\u548C\u7F16\u8F91\u7684\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
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
