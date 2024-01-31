---
title:                "JSONを扱う方法"
date:                  2024-01-19
simple_title:         "JSONを扱う方法"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSONはJavaScript Object Notationの略です。データ交換によく使われます。簡潔で読みやすく、多くの言語で解析・生成が可能なためです。

## How to:
### JSON文字列のパース
```Javascript
let jsonString = '{"name": "Yamada", "age": 30}';
let jsonObj = JSON.parse(jsonString);

console.log(jsonObj.name); // 出力: Yamada
console.log(jsonObj.age);  // 出力: 30
```

### JSON形式の文字列の生成
```Javascript
let person = { name: "Suzuki", age: 25 };
let jsonString = JSON.stringify(person);

console.log(jsonString); // 出力: {"name":"Suzuki","age":25}
```

## Deep Dive
JSONはDouglas Crockfordによって発明されました。XMLがよく使われていた時代の後継として登場。XMLは複雑で重いのに対し、JSONは軽量でスピーディです。JavaScriptのみならず多言語でサポートされています。例えば、Pythonでは`json`モジュール、Javaでは`org.json`ライブラリなどが存在します。

## See Also
- JSON公式サイト：[https://www.json.org/json-ja.html](https://www.json.org/json-ja.html)
- MDNのJSON案内：[https://developer.mozilla.org/ja/docs/Learn/JavaScript/Objects/JSON](https://developer.mozilla.org/ja/docs/Learn/JavaScript/Objects/JSON)
- W3SchoolsのJSONチュートリアル：[https://www.w3schools.com/js/js_json_intro.asp](https://www.w3schools.com/js/js_json_intro.asp)
