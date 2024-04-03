---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:49.611347-07:00
description: "YAML\u306F\u3001\u201CYAML Ain't Markup Language\u201D\u2026"
lastmod: '2024-03-13T22:44:41.976700-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\u3001\u201CYAML Ain't Markup Language\u201D \u306E\u7565\u3067\
  \u3059\u3002\u3053\u308C\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u8A2D\
  \u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u30C7\u30FC\u30BF\u30C0\u30F3\u30D7\u3001\u304A\
  \u3088\u3073\u8A00\u8A9E\u9593\u306E\u30C7\u30FC\u30BF\u4F1D\u9001\u306E\u305F\u3081\
  \u306B\u4F7F\u7528\u3059\u308B\u3001\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\
  \u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u6A19\
  \u6E96\u3067\u3059\u3002\u305D\u306E\u53EF\u8AAD\u6027\u3068\u4F7F\u3044\u3084\u3059\
  \u3055\u304B\u3089\u4EBA\u6C17\u304C\u3042\u308A\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u3084\u30B5\u30FC\u30D3\u30B9\u306E\u8A2D\u5B9A\u306B\u4E00\u822C\
  \u7684\u306B\u9078\u3070\u308C\u307E\u3059\u3002."
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
