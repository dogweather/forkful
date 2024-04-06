---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:49.611347-07:00
description: "\u65B9\u6CD5\uFF1A Java\u3067YAML\u30D5\u30A1\u30A4\u30EB\u3092\u6271\
  \u3046\u306B\u306F\u3001Java Standard\u2026"
lastmod: '2024-04-05T22:38:41.521347-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Java\u3067YAML\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\
  \u306B\u306F\u3001Java Standard Edition\u306B\u306FYAML\u306E\u30B5\u30DD\u30FC\u30C8\
  \u304C\u7D44\u307F\u8FBC\u307E\u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3001\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\
  \u7528\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002\u4EBA\u6C17\u306E\
  \u3042\u308B\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4E00\u3064\u306BSnakeYAML\u304C\
  \u3042\u308A\u3001\u3053\u308C\u3092\u4F7F\u7528\u3059\u308B\u3068YAML\u30C7\u30FC\
  \u30BF\u306E\u89E3\u6790\u3068\u751F\u6210\u3092\u7C21\u5358\u306B\u884C\u3046\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 方法：
JavaでYAMLファイルを扱うには、Java Standard EditionにはYAMLのサポートが組み込まれていないため、サードパーティのライブラリを使用する必要があります。人気のあるライブラリの一つにSnakeYAMLがあり、これを使用するとYAMLデータの解析と生成を簡単に行うことができます。

### SnakeYAMLの設定
まず、プロジェクトにSnakeYAMLを含めます。Mavenを使用している場合は、`pom.xml`に次の依存関係を追加します：

```xml
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.30</version>
</dependency>
```

### YAMLの読み込み
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
`config.yml`が次のようになっていると仮定します：
```yaml
name: Example
version: 1.0
features:
  - login
  - signup
```
出力は以下のようになります：
```
{name=Example, version=1.0, features=[login, signup]}
```

### YAMLの書き込み
JavaオブジェクトからYAMLを生成するには、SnakeYAMLが提供する`dump`メソッドを使用します：
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
これにより、次のYAMLコンテンツが生成され、印刷されます：
```yaml
name: Example
version: 1.0
features:
- login
- signup
```
SnakeYAMLを活用することで、Java開発者は簡単にアプリケーションにYAMLの解析と生成を統合でき、設定とデータ交換の目的でYAMLの可読性と簡潔さから利益を得ることができます。
