---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:43.564928-07:00
description: "YAML\uFF0C\u5373\u201CYAML Ain't Markup Language\uFF08YAML\u4E0D\u662F\
  \u6807\u8BB0\u8BED\u8A00\uFF09\u201D\uFF0C\u662F\u4E00\u79CD\u4F9B\u7A0B\u5E8F\u5458\
  \u7528\u4E8E\u914D\u7F6E\u6587\u4EF6\u3001\u6570\u636E\u8F6C\u50A8\u548C\u8BED\u8A00\
  \u95F4\u6570\u636E\u4F20\u8F93\u7684\u53EF\u8BFB\u6027\u6570\u636E\u5E8F\u5217\u5316\
  \u6807\u51C6\u3002\u7531\u4E8E\u5B83\u7684\u53EF\u8BFB\u6027\u548C\u6613\u7528\u6027\
  \uFF0CYAML\u6210\u4E3A\u914D\u7F6E\u5E94\u7528\u7A0B\u5E8F\u548C\u670D\u52A1\u7684\
  \u5E38\u7528\u9009\u62E9\u3002"
lastmod: '2024-03-13T22:44:47.647724-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u5373\u201CYAML Ain't Markup Language\uFF08YAML\u4E0D\u662F\u6807\
  \u8BB0\u8BED\u8A00\uFF09\u201D\uFF0C\u662F\u4E00\u79CD\u4F9B\u7A0B\u5E8F\u5458\u7528\
  \u4E8E\u914D\u7F6E\u6587\u4EF6\u3001\u6570\u636E\u8F6C\u50A8\u548C\u8BED\u8A00\u95F4\
  \u6570\u636E\u4F20\u8F93\u7684\u53EF\u8BFB\u6027\u6570\u636E\u5E8F\u5217\u5316\u6807\
  \u51C6\u3002\u7531\u4E8E\u5B83\u7684\u53EF\u8BFB\u6027\u548C\u6613\u7528\u6027\uFF0C\
  YAML\u6210\u4E3A\u914D\u7F6E\u5E94\u7528\u7A0B\u5E8F\u548C\u670D\u52A1\u7684\u5E38\
  \u7528\u9009\u62E9\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 什么和为什么？
YAML，即“YAML Ain't Markup Language（YAML不是标记语言）”，是一种供程序员用于配置文件、数据转储和语言间数据传输的可读性数据序列化标准。由于它的可读性和易用性，YAML成为配置应用程序和服务的常用选择。

## 如何操作：
在Java中，由于Java标准版不包括对YAML的内置支持，您可以使用第三方库来操作YAML文件。一个流行的库是SnakeYAML，它允许轻松解析和生成YAML数据。

### 设置SnakeYAML
首先，将SnakeYAML包含到您的项目中。如果您使用Maven，请将以下依赖项添加到您的`pom.xml`中：

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### 读取YAML
```java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class ReadYamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        try (InputStream inputStream = ReadYamlExample.class
                .getClassLoader()
                .getResourceAsStream("config.yml")) {
            Map<String, Object> data = yaml.load(inputStream);
            System.out.println(data);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
假设`config.yml`看起来像这样：
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
输出将是：
```
{name=Example, version=1.0, features=[login, signup]}
```

### 写入YAML
要从Java对象生成YAML，请使用SnakeYAML提供的`dump`方法：
```java
import org.yaml.snakeyaml.Yaml;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

public class WriteYamlExample {
    public static void main(String[] args) {
        Map<String, Object> data = new LinkedHashMap<>();
        data.put("name", "Example");
        data.put("version", 1.0);
        data.put("features", Arrays.asList("login", "signup"));

        Yaml yaml = new Yaml();
        String output = yaml.dump(data);
        System.out.println(output);
    }
}
```
这将生成并打印以下YAML内容：
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
通过利用SnakeYAML，Java开发者可以轻松地将YAML解析和生成集成到他们的应用程序中，从而受益于YAML在配置和数据交换目的上的可读性和简洁性。
