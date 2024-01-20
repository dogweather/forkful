---
title:                "「CSVで働く」"
html_title:           "Javascript: 「CSVで働く」"
simple_title:         "「CSVで働く」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## CSVとは？

CSVとは、「Comma-Separated Values」の略で、データをコンマで区切って表現するファイル形式のことを指します。プログラマーがCSVを扱うのは、データを扱う上でよく使われる形式であるためです。

## 使い方：

CSVファイルを読み込むためには、まずモジュールをインストールする必要があります。例えば、 [csv-parser](https://www.npmjs.com/package/csv-parser) はNode.jsでよく使われるモジュールの一つです。

```Javascript
const csv = require('csv-parser');
const fs = require('fs');

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (row) => {
    console.log(row);
  })
  .on('end', () => {
    console.log('CSVファイルの読み込みが完了しました！');
  });
```

出力は次のようになります：

```Javascript
{ name: 'John', age: '25', country: 'USA' }
{ name: 'Emily', age: '30', country: 'Canada' }
{ name: 'David', age: '28', country: 'Australia' }
```

## 深堀り：

CSVは1970年代に誕生し、当時は英字テキストファイルの形式で使用されていました。現在では、Excelなどの表計算ソフトでもCSV形式でのデータのエクスポートができるため、データの共有や移動に便利です。

CSVの代替としては、JSONやXMLなどがありますが、特に大量のテキストデータを扱う場合はCSVの方が処理が早くなることが多いです。

CSVの実装には、他にも [Papa Parse](https://www.papaparse.com/) や [D3.js](https://d3js.org/) などのライブラリがあります。