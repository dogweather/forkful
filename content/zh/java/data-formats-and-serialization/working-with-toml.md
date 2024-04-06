---
date: 2024-01-26 04:23:07.183110-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4F60\u9700\u8981\u4E00\u4E2ATOML\u89E3\
  \u6790\u5E93\u3002\u6211\u63A8\u8350\u4F7F\u7528`toml4j`\u3002\u50CF\u8FD9\u6837\
  \u5C06\u5B83\u6DFB\u52A0\u5230\u4F60\u7684\u9879\u76EE\u4E2D\uFF1A."
lastmod: '2024-04-05T22:38:46.811179-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4F60\u9700\u8981\u4E00\u4E2ATOML\u89E3\u6790\
  \u5E93\u3002\u6211\u63A8\u8350\u4F7F\u7528`toml4j`\u3002\u50CF\u8FD9\u6837\u5C06\
  \u5B83\u6DFB\u52A0\u5230\u4F60\u7684\u9879\u76EE\u4E2D\uFF1A."
title: "\u4F7F\u7528TOML"
weight: 39
---

## 如何操作：
你需要一个TOML解析库。我推荐使用`toml4j`。像这样将它添加到你的项目中：

```java
// 将此添加至你的build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

以下是如何解析一个TOML文件：

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("服务器IP: " + ip);
        System.out.println("服务器端口: " + port);
    }
}
```

示例输出：

```
服务器IP: 192.168.1.1
服务器端口: 80
```

## 深入了解
TOML由GitHub联合创始人Tom Preston-Werner开发，旨在比XML更简单，比YAML更具体。其最新版本1.0.0在2021年发布，提供了一组稳定的功能。

类似JSON或YAML的替代产品也很受欢迎。JSON非常适合数据交换。YAML对于复杂配置来说更易于人类阅读。TOML的优势在于它的直白性以及在Rust社区中的应用。

在实现方面，当使用Java处理TOML时，请记住你选择的解析器很重要。除了`toml4j`外，一些人会选择`jackson-dataformat-toml`。它们每个都有其细微差别，如错误处理或解析性能，因此基于你项目的需求进行选择。

## 另请参阅
- TOML规范: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
