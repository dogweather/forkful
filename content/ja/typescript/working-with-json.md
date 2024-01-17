---
title:                "「JSONを扱う」"
html_title:           "TypeScript: 「JSONを扱う」"
simple_title:         "「JSONを扱う」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## 何をするのか？ & なぜするのか？
JSON とは、JavaScript Object Notation の略で、データのフォーマット方法のことを指します。プログラマーが JSON と一緒に作業する理由は、データを簡単に読み書きできるようにするためです。

## 作業方法：
```TypeScript
// JSONを使用するための準備
import * as jsonData from './sample.json';

// JSONファイルの作成
const data = {
  name: 'John',
  age: 25,
  interests: ['sports', 'music', 'reading']
};

// JSONファイルの読み込み
console.log(jsonData.name); // John

// JSONファイルの書き込み
import fs from 'fs';
fs.writeFileSync('./sample.json', JSON.stringify(data, null, 2));

// 出力例
{
  "name": "John",
  "age": 25,
  "interests": [
    "sports",
    "music",
    "reading"
  ]
}
```

## 深堀り：
JSON は、1990年代後半に JavaScript の一部として開発されました。他のデータフォーマットである XML や CSV と比べて、可読性が高く、扱いやすいという特徴があります。また、XML や CSV に比べてコード量が少なくて済むため、プログラマーにとっても便利です。JSON に似た形式である YAML もありますが、JSON の方が広く使用されています。

## 関連情報を見る：
- JSON 公式サイト: https://www.json.org/json-ja.html
- TypeScript 公式サイト: https://www.typescriptlang.org/
- YAML 公式サイト: https://yaml.org/