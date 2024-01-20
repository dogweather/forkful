---
title:                "「csvファイルの操作方法」"
html_title:           "TypeScript: 「csvファイルの操作方法」"
simple_title:         "「csvファイルの操作方法」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## 何かについて

CSVとは、Comma-Separated Valuesの略称であり、データをテキストファイルに表現するためのフォーマットです。プログラマーがCSVを使用する理由は、データを表形式で扱いやすくするためです。データベースに格納するためのデータ変換やデータのエクスポート、インポートなどに利用されます。

## 使い方

```TypeScript
import * as fs from 'fs';

// CSV形式のデータを読み込む
const csv = fs.readFileSync('sample.csv', {encoding: 'utf8'});

// データを配列に変換する
const dataArray = csv.split('\n').map(data => data.split(','));

// 配列の各要素を出力する
for (let data of dataArray) {
  console.log(data);
}
```
出力結果：
```
["1", "John", "Smith"]
["2", "Jane", "Doe"]
["3", "Bob", "Johnson"]
```

## 詳細を知る

CSVは1970年代に開発されたデータフォーマットであり、現在でも多くのアプリケーションで使用されています。代替としては、Tab-separated values (TSV)やJSON形式などがあります。CSVはテキストファイルとして扱われるため、エクセルやテキストエディタから簡単に編集できます。しかし、データ内にカンマや改行が含まれると解析が困難になるため、注意が必要です。

## 関連リンク

- [JSON形式とは？](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/JSON)