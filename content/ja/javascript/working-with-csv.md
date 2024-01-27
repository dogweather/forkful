---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
CSVとはComma-Separated Valuesの略です。テキストデータを簡単な形式で扱う手法です。CSVを使う理由は、Excelやデータベースとの相互運用性が高く、操作が直感的であるためです。

## How to: (実装方法)
JavaScriptでCSVを扱う基本的な例を示します。

```javascript
// CSV文字列の解析
const csv = `ID,Name,Age
1,Taro,20
2,Hanako,25`;

// CSVを行に分ける
const rows = csv.split("\n");

// 各行をカンマで分割してオブジェクトに変換
const data = rows.slice(1).map(row => {
  const [id, name, age] = row.split(',');
  return { id, name, age };
});

console.log(data);
```

出力例:
```javascript
[
  { id: '1', name: 'Taro', age: '20' },
  { id: '2', name: 'Hanako', age: '25' }
]
```

## Deep Dive (深掘り)
CSVは1972年にIBMが開発。テキストベースでありながら、表のようなデータ構造を持つことができるシンプルなフォーマットです。代替手段にはJSONやXMLがありますが、CSVはそのシンプルさから広く使われています。JavaScriptでは`split()`メソッドや正規表現、ライブラリ(PapaParseなど)を使ってCSVを扱います。

## See Also (関連情報)
- MDN Web Docs（CSVの扱い方の基礎）: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split
- PapaParse（JavaScript用CSVパーサー）: https://www.papaparse.com/
- RFC 4180（CSVの仕様）: https://tools.ietf.org/html/rfc4180
