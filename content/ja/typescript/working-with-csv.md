---
title:                "「CSVを扱う」"
html_title:           "TypeScript: 「CSVを扱う」"
simple_title:         "「CSVを扱う」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

CSVファイルを処理することは、データを管理したり、上位プログラムとのデータ交換を行ったりする際に非常に便利です。TypeScriptを使用すると、型安全性や使いやすさを備えた柔軟なCSV処理が可能です。

## How To

```typescript
import * as fs from 'fs';
import * as csv from 'csv-parser';

// CSVファイルを読み込む
fs.createReadStream('data.csv')
  // csv-parserを使用してデータをオブジェクトに変換する
  .pipe(csv())
  .on('data', (data) => {
    // データをコンソールに出力する
    console.log(data);
  })
  .on('end', () => {
    // 処理終了時にメッセージを出力する
    console.log('CSVデータの読み込みが完了しました。');
  });
```

実行結果:
```
{ id: 1, name: 'John', age: 30 }
{ id: 2, name: 'Jane', age: 27 }
{ id: 3, name: 'Bob', age: 35 }
```

## Deep Dive

CSVデータはファイルが単純であるため、データの変換や加工が簡単に行えます。また、CSVパーサーライブラリを使用することで、セルや行のデータを扱いやすいオブジェクト形式に変換できます。さらに、TypeScriptを使用することで、データの型安全性を確保しながら柔軟な処理が可能です。

## See Also

- [CSV Parser](https://www.npmjs.com/package/csv-parser)
- [Node.js fs module](https://nodejs.org/api/fs.html)