---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
用途和原因？
YAML是一种数据序列化格式，常用于配置文件和数据交换。程序员使用YAML因为它简单明了，易于人类阅读和编写。

## How to:
如何操作：
下面是一些基本的PowerShell操作YAML的例子：

```PowerShell
# 安装必要的YAML模块
Install-Module -Name powershell-yaml

# 读取YAML文件
$yamlContent = Get-Content -Path 'example.yaml' | Out-String
$data = ConvertFrom-Yaml $yamlContent
$data

# 创建YAML内容并写入文件
$newData = @{
  name = '张三'
  age = 30
  languages = @('PowerShell', 'Python')
}
$newYaml = ConvertTo-Yaml $newData
$newYaml | Out-File -FilePath 'newExample.yaml'

# 输出示例
$newYaml
```

## Deep Dive:
深入了解：
YAML，全称是“YAML Ain't Markup Language”（原义为“Yet Another Markup Language”），起源于2001年。与JSON和XML相比，YAML的可读性更强。但是在复杂性和递归结构处理上，JSON和XML提供的支持可能更好。在PowerShell中处理YAML主要依赖于外部模块，如`powershell-yaml`。

## See Also:
其他资源：
- YAML 官方网站: [https://yaml.org/](https://yaml.org/)
- `powershell-yaml` 模块：[https://github.com/cloudbase/powershell-yaml](https://github.com/cloudbase/powershell-yaml)
- YAML 和 JSON 对比：[https://en.wikipedia.org/wiki/JSON#Comparison_with_YAML](https://en.wikipedia.org/wiki/JSON#Comparison_with_YAML)
