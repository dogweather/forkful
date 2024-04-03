---
date: 2024-01-26 04:31:03.617718-07:00
description: "\u5904\u7406 XML \u610F\u5473\u7740\u5728\u4E00\u4E2A\u666E\u904D\u5B58\
  \u5728\u7684\u3001\u7ED3\u6784\u5316\u7684\u683C\u5F0F\u4E2D\u6574\u7406\u6570\u636E\
  \uFF0C\u8FD9\u79CD\u683C\u5F0F\u7528\u4E8E\u914D\u7F6E\u3001\u901A\u4FE1\u7B49\u591A\
  \u4E2A\u65B9\u9762\u3002\u7A0B\u5E8F\u5458\u64CD\u7EB5 XML \u6765\u8BFB\u53D6\u3001\
  \u5199\u5165\u3001\u66F4\u65B0\u548C\u67E5\u8BE2\u6570\u636E\u2014\u2014\u8FD9\u5BF9\
  \u4E8E\u6570\u4EE5\u4E07\u8BA1\u7684\u5E94\u7528\u7A0B\u5E8F\u548C\u670D\u52A1\u4E2D\
  \u7684\u4E92\u64CD\u4F5C\u6027\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:48.293571-06:00'
model: gpt-4-0125-preview
summary: "\u5904\u7406 XML \u610F\u5473\u7740\u5728\u4E00\u4E2A\u666E\u904D\u5B58\u5728\
  \u7684\u3001\u7ED3\u6784\u5316\u7684\u683C\u5F0F\u4E2D\u6574\u7406\u6570\u636E\uFF0C\
  \u8FD9\u79CD\u683C\u5F0F\u7528\u4E8E\u914D\u7F6E\u3001\u901A\u4FE1\u7B49\u591A\u4E2A\
  \u65B9\u9762\u3002\u7A0B\u5E8F\u5458\u64CD\u7EB5 XML \u6765\u8BFB\u53D6\u3001\u5199\
  \u5165\u3001\u66F4\u65B0\u548C\u67E5\u8BE2\u6570\u636E\u2014\u2014\u8FD9\u5BF9\u4E8E\
  \u6570\u4EE5\u4E07\u8BA1\u7684\u5E94\u7528\u7A0B\u5E8F\u548C\u670D\u52A1\u4E2D\u7684\
  \u4E92\u64CD\u4F5C\u6027\u81F3\u5173\u91CD\u8981\u3002."
title: "\u5904\u7406XML"
weight: 40
---

## 如何操作：
Fish 没有内置的 XML 解析功能，因此你需要依赖外部工具如 `xmllint` 或 `xmlstarlet`。以下是一个读取值的代码片段：

```fish
# 使用 xmlstarlet 解析 XML
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

输出：
```
Hello World
```

要编辑 XML，使用这个：

```fish
# 使用 xmlstarlet 编辑 XML 元素
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

输出：
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## 深入探讨：
XML 自 90 年代末期以来就存在了，它被设计为易读和对机器友好。虽然 JSON 因其简单性而取代了一些 XML 的热度，但在文档验证和命名空间至关重要的地方，XML 仍然根深蒂固。

有替代方案吗？当然——JSON、YAML，或甚至是像 Protocol Buffers 这样的二进制格式，适用于那些性能密集型的应用程序。但是，XML 的模式和 XSLT（用于 XML 转换）可以是复杂场景中关注稳健性时的决定性因素。

在底层，像 `xmlstarlet` 这样的工具包装了强大的库，比如 libxml2，为你提供 XPath 和 XQuery 以进行细致的 XML 调整。这些不仅仅是 XML 工具，而是通往 DOM 操作的大门，因为你会在任何处理 XML 的语言中应用类似的概念。

## 另请参见：
- [xmlstarlet 文档](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fish 文档](https://fishshell.com/docs/current/index.html)
- [XPath 和 XQuery 函数与操作符](https://www.w3.org/TR/xpath-functions/)
