---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSONはデータを保存・転送するフォーマット。軽く、読み書きしやすく、多言語対応で人気。

## How to:
JavaでJSONを扱うには`org.json`か`com.google.gson`ライブラリ使うのが定番。例を見よう。

```java
import org.json.JSONObject;

public class JsonExample {
    public static void main(String[] args) {
        // JSONオブジェクトを作成
        JSONObject obj = new JSONObject();
        obj.put("name", "Yamada");
        obj.put("age", 25);

        // JSON文字列に変換
        String jsonStr = obj.toString();
        System.out.println(jsonStr);
        
        // JSON文字列からオブジェクトを読み込む
        JSONObject obj2 = new JSONObject(jsonStr);
        System.out.println("Name: " + obj2.getString("name"));
        System.out.println("Age: " + obj2.getInt("age"));
    }
}
```
出力:
```
{"name":"Yamada","age":25}
Name: Yamada
Age: 25
```

## Deep Dive
JSON（JavaScript Object Notation）は、2001年にJavaScript内のオブジェクト表記法から派生。REST APIや設定ファイルで定番。XMLやYAMLが代替えとしてあるが、JSONはパースが速く、書式がシンプル。Javaでは`javax.json`やJacksonなど、さまざまな実装があります。

## See Also
- JSON.org（概要と文法）: [http://json.org/](http://json.org/)
- Google Gson GitHubページ（ドキュメントとダウンロード）: [https://github.com/google/gson](https://github.com/google/gson)
- JSON in Java（org.jsonライブラリのリファレンス）: [https://stleary.github.io/JSON-java/](https://stleary.github.io/JSON-java/)
