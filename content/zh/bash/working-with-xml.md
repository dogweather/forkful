---
title:                "处理XML"
aliases:
- zh/bash/working-with-xml.md
date:                  2024-01-26T04:27:46.038526-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么及为什么？
处理XML涉及解析、提取和操作可扩展标记语言（XML）格式的数据。编程人员之所以要处理XML，是因为它是一种广泛使用的数据交换格式，用于配置、API等。

## 如何操作：
以下是在Bash中解析XML的方法。使用工具？xmllint和xmlstarlet。遍历XML元素？当然可以。示例与样本输出：

```bash
# 假设已安装xmlstarlet
# 通过以下命令安装：apt-get install xmlstarlet

# 解析XML内容
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# 使用xmlstarlet提取名称
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# 输出应当为：
# Apple
# Banana
```

## 深入了解
回到90年代，XML作为SGML的更简单替代品出现，但比HTML更有结构。现在，它有了伴侣——例如JSON、YAML。但XML仍在发挥作用，尤其是在配置和基于SOAP的网络服务中。

在工具方面，xmllint适用于XML验证、xpath查询。xmlstarlet是XML操作的多功能刀——查询、编辑、验证、转换。在bash脚本中，它们是XML任务的超级英雄。

内部机制方面，xmllint使用libxml2——XML C解析器。它速度快，但错误消息？难以理解。至于xmlstarlet？递归模板和EXSLT支持。思维挑战，但功能强大。

## 另请参阅
- [xmlsoft.org](http://xmlsoft.org/)：Libxml2和xmllint相关内容。
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash)：现实世界的问题和解决方案。
- [W3Schools XML教程](https://www.w3schools.com/xml/)：XML的基础知识。
