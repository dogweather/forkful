---
title:                "JSONを扱う方法"
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
JSONはデータフォーマットです。プログラマーはデータ交換のため、そして設定やAPI応答を扱うためにJSONを使います。

## How to: / どうやって：
```TypeScript
// JSON文字列をパースする
const jsonString: string = '{"name": "Taro", "age": 30}';
const userData: { name: string; age: number } = JSON.parse(jsonString);
console.log(userData.name); // Taro

// オブジェクトをJSON文字列に変換する
const userObject: { name: string; age: number } = { name: "Hanako", age: 25 };
const jsonOutput: string = JSON.stringify(userObject);
console.log(jsonOutput); // {"name":"Hanako","age":25}
```

## Deep Dive / 探求:
JSON（JavaScript Object Notation）は軽量なデータ交換フォーマット。1999年にJavaScript内で生まれましたが、言語非依存で広く使われています。XMLはもう一つの代替手段ですが、より煩雑です。TypeScriptで扱うとき、型定義により安全なコーディングが可能です。

## See Also / 関連する情報:
- MDN Web Docs JSON: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/JSON
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
- JSON vs XML: https://www.w3schools.com/js/js_json_xml.asp
