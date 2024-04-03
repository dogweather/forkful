---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:24.529015-07:00
description: "Java\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u306E\
  JSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:41.977543-06:00'
model: gpt-4-0125-preview
summary: "Java\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u306EJSON\uFF08\
  JavaScript Object Notation\uFF09\u306E\u53D6\u308A\u6271\u3044\u306F\u3001\u3053\
  \u306E\u8EFD\u91CF\u306A\u30C7\u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u3092\u6271\u3046\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001JSON\u3092\u4F7F\u3063\u3066\u69CB\u9020\
  \u5316\u3055\u308C\u305F\u30C7\u30FC\u30BF\u3092\u30CD\u30C3\u30C8\u30EF\u30FC\u30AF\
  \u8D8A\u3057\u306B\u30B7\u30EA\u30A2\u30E9\u30A4\u30BA\uFF08\u76F4\u5217\u5316\uFF09\
  \u3057\u8EE2\u9001\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u7C21\u5358\u306B\
  \u8A2D\u5B9A\u30FB\u4FDD\u5B58\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u63A1\
  \u7528\u3057\u3066\u3044\u307E\u3059\u3002\u305D\u308C\u306F\u3001JSON\u304C\u4EBA\
  \u9593\u306B\u3068\u3063\u3066\u8AAD\u307F\u3084\u3059\u304F\u8A00\u8A9E\u306B\u4F9D\
  \u5B58\u3057\u306A\u3044\u304B\u3089\u3067\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
