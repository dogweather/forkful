---
title:                "「JSONを使ったプログラミング」"
html_title:           "Swift: 「JSONを使ったプログラミング」"
simple_title:         "「JSONを使ったプログラミング」"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-json.md"
---

{{< edit_this_page >}}

## 何かしら？何のために？
JSONとは、プログラミングにおいてデータを扱う際に使用される一般的な形式の一つです。プログラマーたちは、JSONを使用することでデータを簡単に処理することができます。

## 使い方：
以下に、JSONを取り扱うためのコード例とその出力を示します。

```Swift
// JSONをデコードする
let jsonString = """
{
    "name": "Taro",
    "age": 25,
    "city": "Tokyo"
}
""".data(using: .utf8)!

struct Person: Codable {
    let name: String
    let age: Int
    let city: String
}

let decoder = JSONDecoder()
let personData = try decoder.decode(Person.self, from: jsonString)
print(personData.name) // 出力結果：Taro
print(personData.age) // 出力結果：25
print(personData.city) // 出力結果：Tokyo
```

```Swift
// JSONをエンコードする
struct Person: Codable {
    let name: String
    let age: Int
    let city: String
}

let person = Person(name: "Hanako", age: 28, city: "Osaka")
let encoder = JSONEncoder()
let encodedPerson = try encoder.encode(person)

print(String(data: encodedPerson, encoding: .utf8)!) // 出力結果：{"name": "Hanako", "age": 28, "city": "Osaka"}
```

## もっと深く掘り下げる：
歴史的な文脈としては、JSONは2000年代初頭にJavaScriptオブジェクト記法から派生したものです。代替として使用されたのは、XMLやCSVなどがありますが、JSONは扱いやすさと可読性が高いため、今日では非常に一般的に使用されています。JSONは、iOSやmacOSといったApple製品においても広く使用されています。

## 関連情報：
- [Swift Documentation - Codable](https://developer.apple.com/documentation/foundation/archives_and_serialization/encoding_and_decoding_custom_types)
- [Swift Documentation - Encoding and Decoding](https://developer.apple.com/documentation/foundation/archives_and_serialization/encoding_and_decoding_custom_types)
- [The JSON Data Interchange Format](https://tools.ietf.org/html/rfc7159)