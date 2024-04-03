---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:43.564928-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Java\u4E2D\uFF0C\u7531\u4E8EJava\u6807\
  \u51C6\u7248\u4E0D\u5305\u62EC\u5BF9YAML\u7684\u5185\u7F6E\u652F\u6301\uFF0C\u60A8\
  \u53EF\u4EE5\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u6765\u64CD\u4F5CYAML\u6587\u4EF6\
  \u3002\u4E00\u4E2A\u6D41\u884C\u7684\u5E93\u662FSnakeYAML\uFF0C\u5B83\u5141\u8BB8\
  \u8F7B\u677E\u89E3\u6790\u548C\u751F\u6210YAML\u6570\u636E\u3002 #."
lastmod: '2024-03-13T22:44:47.647724-06:00'
model: gpt-4-0125-preview
summary: "\u5728Java\u4E2D\uFF0C\u7531\u4E8EJava\u6807\u51C6\u7248\u4E0D\u5305\u62EC\
  \u5BF9YAML\u7684\u5185\u7F6E\u652F\u6301\uFF0C\u60A8\u53EF\u4EE5\u4F7F\u7528\u7B2C\
  \u4E09\u65B9\u5E93\u6765\u64CD\u4F5CYAML\u6587\u4EF6\u3002\u4E00\u4E2A\u6D41\u884C\
  \u7684\u5E93\u662FSnakeYAML\uFF0C\u5B83\u5141\u8BB8\u8F7B\u677E\u89E3\u6790\u548C\
  \u751F\u6210YAML\u6570\u636E."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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
