---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？

CSVファイルを扱うって、テキスト形式でデータを保存してあるんだ。プログラマは、データのやり取りや保存のためによく使う。簡単で、ほかのプログラムやプラットフォームとも互換性が高いからね。

## How to / 方法

以下に、TypeScriptでCSVを読み書きする方法の例を示す。まずは、Node.jsの`fs`モジュールと、`csv-parse`と`csv-stringify`ライブラリを使ってみよう：

```typescript
// 必要なライブラリをインポート
import fs from 'fs';
import parse from 'csv-parse/lib/sync';
import stringify from 'csv-stringify';

// CSV読み込み
const input = fs.readFileSync('sample.csv', 'utf-8');
const records = parse(input, {
  columns: true,
  skip_empty_lines: true
});
console.log(records);

// CSV書き込み
const output = [];
records.forEach((record) => {
  output.push({id: record.id, name: record.name.toUpperCase()});
});

stringify(output, {
  header: true
}, (err, output) => {
  if (err) throw err;
  fs.writeFileSync('output.csv', output);
});
```

**サンプル出力**：

```
[ { id: '1', name: 'YAMADA' }, { id: '2', name: 'TANAKA' } ]
```

## Deep Dive / 深掘り

CSVは文字データだけを扱うが、1970年代から使われている老舗形式だ。代替としては、XMLやJSONがあるが、単純なデータ構造のときはCSVの方が手軽。ライブラリによってはパフォーマンスが異なるため、目的に最適なものを選ぼう。

## See Also / 関連情報

- CSV標準: [RFC 4180](https://datatracker.ietf.org/doc/html/rfc4180)
- `csv-parse`ライブラリ: [csv-parse](https://www.npmjs.com/package/csv-parse)
- `csv-stringify`ライブラリ: [csv-stringify](https://www.npmjs.com/package/csv-stringify)
- 別のCSVライブラリ: [PapaParse](https://www.papaparse.com/)
- Node.jsの`fs`モジュール: [File System | Node.js v17.4.0 Documentation](https://nodejs.org/api/fs.html)
