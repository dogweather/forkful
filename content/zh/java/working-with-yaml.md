---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
什么是 YAML？YAML 是 "YAML Ain't Markup Language"（YAML 不是标记语言）的递归缩写，用于配置文件和数据交换。为何使用 YAML？它易读，结构清晰，广泛用于配置文件、数据存储和跨语言数据分享。

## How to:
要在 Java 中使用 YAML，首先确保引入了 YAML 解析库，如 `snakeyaml`。以下是一个简单的例子。

```Java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.util.Map;

public class YamlExample {
    public static void main(String[] args) {
        Yaml yaml = new Yaml();
        InputStream inputStream = YamlExample.class
            .getClassLoader()
            .getResourceAsStream("config.yaml");
        Map<String, Object> data = yaml.load(inputStream);
        System.out.println(data);
    }
}
```

`config.yaml` 文件:
```yaml
database:
  host: localhost
  port: 3306
```

输出:
```plaintext
{database={host=localhost, port=3306}}
```

## Deep Dive
YAML 诞生于 2001 年，目的是更易读易用。JSON 和 XML 都是替代格式，但 YAML 特别适合配置文件。在 Java 中处理 YAML，通常通过 `snakeyaml` 库来解析和生成 YAML 数据，但也有其他库，如 `jackson-dataformat-yaml`.

## See Also
- YAML 官网: https://yaml.org/
- SnakeYAML 官方文档: https://bitbucket.org/asomov/snakeyaml/wiki/Documentation
- Jackson 数据格式库: https://github.com/FasterXML/jackson-dataformats-text/tree/master/yaml
