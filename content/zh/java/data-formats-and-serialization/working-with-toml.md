---
aliases:
- /zh/java/working-with-toml/
date: 2024-01-26 04:23:07.183110-07:00
description: "TOML\u4EE3\u8868Tom\u7684\u660E\u663E\u7684\u3001\u6700\u5C0F\u5316\u7684\
  \u8BED\u8A00\u3002\u5B83\u662F\u4E00\u79CD\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\u7684\
  \u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\
  \u4F7F\u7528\u5B83\uFF0C\u662F\u56E0\u4E3A\u5B83\u6613\u4E8E\u8BFB\u5199\uFF0C\u5E76\
  \u4E14\u80FD\u591F\u5F88\u597D\u5730\u6620\u5C04\u5230\u54C8\u5E0C\u8868\u4E0A\u3002"
lastmod: 2024-02-18 23:08:59.042046
model: gpt-4-0125-preview
summary: "TOML\u4EE3\u8868Tom\u7684\u660E\u663E\u7684\u3001\u6700\u5C0F\u5316\u7684\
  \u8BED\u8A00\u3002\u5B83\u662F\u4E00\u79CD\u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\u7684\
  \u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\
  \u4F7F\u7528\u5B83\uFF0C\u662F\u56E0\u4E3A\u5B83\u6613\u4E8E\u8BFB\u5199\uFF0C\u5E76\
  \u4E14\u80FD\u591F\u5F88\u597D\u5730\u6620\u5C04\u5230\u54C8\u5E0C\u8868\u4E0A\u3002"
title: "\u4F7F\u7528TOML"
---

{{< edit_this_page >}}

## 什么 & 为什么？
TOML代表Tom的明显的、最小化的语言。它是一种用于配置文件的数据序列化格式。程序员之所以使用它，是因为它易于读写，并且能够很好地映射到哈希表上。

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
