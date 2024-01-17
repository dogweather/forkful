---
title:                "JSONを使う"
html_title:           "Java: JSONを使う"
simple_title:         "JSONを使う"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-json.md"
---

{{< edit_this_page >}}

## なに？なぜ？

JSONとは、JavaScript Object Notationのことで、データをハイレベルで扱えるようにするために使用されるフォーマットです。プログラマーたちは、JSONを使用してデータを交換したり、保存したりすることができるようになります。

## 使い方：

```Java
// JSONライブラリをインポートする
import org.json.*;

// Mapを作成する
Map<String, String> member = new HashMap<>();
member.put("name", "Bob");
member.put("age", "25");
member.put("occupation", "programmer");

// MapをJSON形式に変換する
JSONObject json = new JSONObject(member);

// JSONを表示する
System.out.println("Member: " + json);

// JSONからデータを取得する
String name = json.getString("name");
int age = json.getInt("age");
String occupation = json.getString("occupation");

// 取得したデータを表示する
System.out.println("Name: " + name);
System.out.println("Age: " + age);
System.out.println("Occupation: " + occupation);
```

出力結果は以下のようになります：

```
Member: {"name": "Bob", "age": 25, "occupation": "programmer"}
Name: Bob
Age: 25
Occupation: programmer
```

## 詳しく見る：

JSONは、2001年にDouglas Crockfordによって作成されました。XMLやCSVのような他のデータフォーマットに比べて、書きやすい構文を持つことで人気があります。JSONの代替として、XMLやCSVなどがありますが、JSONはユーザーにとってより読みやすく、処理も速いと言われています。

JSONは、プログラミング言語で直接サポートされています。Javaでは、JSONを扱うためのライブラリが提供されています。

## 関連リンク：

- [JSON公式サイト](https://www.json.org/)
- [JSON Wikipediaページ](https://ja.wikipedia.org/wiki/JSON)
- [JavaでJSONを扱う方法のチュートリアル](https://www.baeldung.com/java-org-json)