---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

category:             "Swift"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSONとは、JavaScript Object Notationの略で、データをテキスト形式でやり取りするための標準フォーマットです。Swiftプログラマーは、Web APIからデータを取得したり、アプリ間でデータを交換したりする際にJSONを扱います。

## How to:
```Swift
import Foundation

// JSON文字列
let jsonString = """
{
    "name": "Taro",
    "age": 30,
    "isProgrammer": true
}
"""
// JSONデコード
struct Person: Codable {
    var name: String
    var age: Int
    var isProgrammer: Bool
}

if let jsonData = jsonString.data(using: .utf8) {
    do {
        let person = try JSONDecoder().decode(Person.self, from: jsonData)
        print(person) // 出力: Person(name: "Taro", age: 30, isProgrammer: true)
    } catch {
        print(error)
    }
}
```

## Deep Dive
JSONは軽量で読みやすく、多くの言語で扱えるため、XMLよりも現代的なデータ交換のフォーマットとして好まれています。Swiftでは`Codable`プロトコルが導入されて以降、JSONのシリアライズ・デシリアライズ（エンコード・デコード）は非常にシンプルになりました。代替手段には`PropertyListSerialization`やサードパーティライブラリがありますが、標準ライブラリのサポートは強力です。

## See Also
- [Apple Developer Documentation - Encoding and Decoding Custom Types](https://developer.apple.com/documentation/foundation/archives_and_serialization/encoding_and_decoding_custom_types)
