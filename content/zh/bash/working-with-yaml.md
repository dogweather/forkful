---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
YAML是一种易于阅读的数据序列化格式，广泛用于配置文件和数据交换。程序员使用YAML因为它清晰、直观且易于与多种编程语言兼容。

## 如何操作：
```Bash
# 安装yaml解析工具
sudo apt-get install -y yq

# 读取YAML文件中的特定内容
cat <<EOF > example.yaml
user:
  name: Zhang San
  age: 30
EOF

# 使用yq查找用户名字
echo "User Name: $(yq e '.user.name' example.yaml)"

# 输出示例：
# User Name: Zhang San
```

## 深入了解：
YAML（YAML Ain't Markup Language）起源于2001年，它的设计目标是易于人类阅读和机器解析。尽管JSON和XML是YAML的替代品，但它们并不那么便于阅读。YAML文件的处理依赖于具体的解析器，如Python中的`PyYAML`或Ruby的`Psych`，而在Bash中常用的是`yq`。

## 参见：
- YAML官方网站：[https://yaml.org/](https://yaml.org/)
- yq GitHub仓库：[https://github.com/mikefarah/yq](https://github.com/mikefarah/yq)
- YAML与JSON在线转换器：[https://www.json2yaml.com/](https://www.json2yaml.com/)
