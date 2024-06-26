---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:24.529015-07:00
description: "\u65B9\u6CD5\uFF1A \u305D\u308C\u3067\u306F\u3001Java\u3067JSON\u3092\
  \u4F7F\u3063\u3066\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u3092\u59CB\u3081\u307E\u3057\
  \u3087\u3046\u3002 \u307E\u305A\u3001`Jackson`\u3084`Google Gson`\u306E\u3088\u3046\
  \u306AJSON\u51E6\u7406\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u5FC5\u8981\u3067\u3059\
  \u3002\u3053\u3053\u3067\u306F`Jackson`\u3092\u4F7F\u3044\u307E\u3059\u306E\u3067\
  \u3001\u4EE5\u4E0B\u306E\u4F9D\u5B58\u95A2\u4FC2\u3092`pom.xml`\u306B\u8FFD\u52A0\
  \u3057\u3066\u304F\u3060\u3055\u3044\uFF1A."
lastmod: '2024-04-05T22:38:41.523108-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u305D\u308C\u3067\u306F\u3001Java\u3067JSON\u3092\u4F7F\
  \u3063\u3066\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u3092\u59CB\u3081\u307E\u3057\u3087\
  \u3046\u3002 \u307E\u305A\u3001`Jackson`\u3084`Google Gson`\u306E\u3088\u3046\u306A\
  JSON\u51E6\u7406\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u5FC5\u8981\u3067\u3059\u3002\
  \u3053\u3053\u3067\u306F`Jackson`\u3092\u4F7F\u3044\u307E\u3059\u306E\u3067\u3001\
  \u4EE5\u4E0B\u306E\u4F9D\u5B58\u95A2\u4FC2\u3092`pom.xml`\u306B\u8FFD\u52A0\u3057\
  \u3066\u304F\u3060\u3055\u3044\uFF1A."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
