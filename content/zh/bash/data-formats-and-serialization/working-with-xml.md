---
date: 2024-01-26 04:27:46.038526-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4EE5\u4E0B\u662F\u5728Bash\u4E2D\u89E3\
  \u6790XML\u7684\u65B9\u6CD5\u3002\u4F7F\u7528\u5DE5\u5177\uFF1Fxmllint\u548Cxmlstarlet\u3002\
  \u904D\u5386XML\u5143\u7D20\uFF1F\u5F53\u7136\u53EF\u4EE5\u3002\u793A\u4F8B\u4E0E\
  \u6837\u672C\u8F93\u51FA\uFF1A."
lastmod: '2024-03-13T22:44:47.991302-06:00'
model: gpt-4-0125-preview
summary: "\u4EE5\u4E0B\u662F\u5728Bash\u4E2D\u89E3\u6790XML\u7684\u65B9\u6CD5\u3002\
  \u4F7F\u7528\u5DE5\u5177\uFF1Fxmllint\u548Cxmlstarlet\u3002\u904D\u5386XML\u5143\
  \u7D20\uFF1F\u5F53\u7136\u53EF\u4EE5\u3002\u793A\u4F8B\u4E0E\u6837\u672C\u8F93\u51FA\
  \uFF1A."
title: "\u5904\u7406XML"
weight: 40
---

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
