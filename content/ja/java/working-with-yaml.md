---
title:                "YAML を操作する"
aliases:
- ja/java/working-with-yaml.md
date:                  2024-02-03T19:25:49.611347-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
YAMLは、“YAML Ain't Markup Language” の略です。これは、プログラマーが設定ファイル、データダンプ、および言語間のデータ伝送のために使用する、人間が読みやすいデータシリアライゼーション標準です。その可読性と使いやすさから人気があり、アプリケーションやサービスの設定に一般的に選ばれます。

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
