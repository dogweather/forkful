---
aliases:
- /ja/java/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:24.529015-07:00
description: "Java\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u306E\
  JSON\uFF08JavaScript Object\u2026"
lastmod: 2024-02-18 23:08:54.821661
model: gpt-4-0125-preview
summary: "Java\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u306EJSON\uFF08\
  JavaScript Object\u2026"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
Javaアプリケーション内でのJSON（JavaScript Object Notation）の取り扱いは、この軽量なデータ交換フォーマットを扱うことを意味します。プログラマーは、JSONを使って構造化されたデータをネットワーク越しにシリアライズ（直列化）し転送したり、データを簡単に設定・保存したりするために採用しています。それは、JSONが人間にとって読みやすく言語に依存しないからです。

## 方法：
それでは、JavaでJSONを使ってコーディングを始めましょう。

まず、`Jackson`や`Google Gson`のようなJSON処理ライブラリが必要です。ここでは`Jackson`を使いますので、以下の依存関係を`pom.xml`に追加してください：

```xml
<dependency>
    <groupId>com.fasterxml.jackson.core</groupId>
    <artifactId>jackson-databind</artifactId>
    <version>2.13.1</version>
</dependency>
```

さて、単純なJavaオブジェクトをJSONにシリアライズ（書き込み）しましょう：

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = new Person("Alex", 30);
            String json = mapper.writeValueAsString(person);
            System.out.println(json);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

class Person {
    public String name;
    public int age;

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

出力は次のようになります：

```json
{"name":"Alex","age":30}
```

次に、JSONをJavaオブジェクトにデシリアライズ（読み込み）するには：

```java
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonExample {
    public static void main(String[] args) {
        String json = "{\"name\":\"Alex\",\"age\":30}";
        try {
            ObjectMapper mapper = new ObjectMapper();
            Person person = mapper.readValue(json, Person.class);
            System.out.println(person.name + " is " + person.age + " years old.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

出力は次のようになります：

```
Alex is 30 years old.
```

## 深堀り
JSONのシンプルさと有効性が、ウェブ上のデータ交換のための事実上の標準となり、XMLをその玉座から引きずり下ろしました。JSONは2000年代初頭に導入され、JavaScriptから派生しましたが、現在ではほとんどの言語でサポートされています。

JSONに代わるものには、より冗長なXMLや、サイズと速度ではより効率的ですが、人間には読みにくいバイナリ形式のProtocol BuffersやMessagePackなどがあります。それぞれに使用されるケースがあり、選択は特定のデータニーズとコンテキストに依存します。

Javaでは、`Jackson`や`Gson`のほかに、JSONを扱うための`JsonB`や`org.json`といったライブラリがあります。Jacksonはストリームベースの処理を提供し、その速さで知られています。一方、Gsonは使いやすさで高い評価を受けています。JsonBはJakarta EEの一部で、より標準化されたアプローチを提供します。

JSONを実装する際は、適切に例外を処理することを忘れないでください - あなたのコードは悪い入力に対しても堅牢であるべきです。また、自動データバインディングのセキュリティへの影響を考慮し、常に入力を検証してください！

## 参照
- [Jackson Project](https://github.com/FasterXML/jackson)
- [Gson Project](https://github.com/google/gson)
- [JSON Specification](https://www.json.org/json-en.html)
- [JsonB Specification](https://jakarta.ee/specifications/jsonb/)
