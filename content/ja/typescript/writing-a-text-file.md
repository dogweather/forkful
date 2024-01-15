---
title:                "テキストファイルの書き方"
html_title:           "TypeScript: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why （なぜ）
テキストファイルを作成することに興味がある方は、データを保存し、共有することが簡単で便利だからです。

## How To （作り方）
作成するテキストファイルのタイプスクリプトコードの例を以下に示します。また、出力結果も併記します。

```TypeScript
// テキストファイルを作成するコード
const fs = require('fs');

const text = "これはテストです。"; // ファイルに書き込まれるテキスト
const filename = "test.txt"; // ファイル名

// ファイルを作成し、文字列を書き込む
fs.writeFile(filename, text, (err) => {
    if (err) throw err;
    console.log('ファイルが作成されました。'); // 成功時のメッセージ

    // ファイルを読み込む
    fs.readFile(filename, 'utf8', (err, data) => {
        if (err) throw err;
        console.log(data); // ファイルの中身を表示
    });
});
```

出力結果：

```
ファイルが作成されました。
これはテストです。
```

## Deep Dive （詳細な説明）
テキストファイルを作成する方法は様々ですが、基本的な流れは次の通りです。

1. 必要なライブラリをインポートする
2. 書き込むテキストを用意する
3. ファイル名を指定する
4. ファイルを作成し、テキストを書き込む
5. ファイルを読み込んで中身を表示する

テキストファイルは、テキストエディターを使って作成することもできますが、プログラムで作成することで自動化することができます。

## See Also （関連リンク）
- [Node.js ドキュメント](https://nodejs.org/ja/docs/)
- [ファイルシステムを使ったファイルの読み込みと書き込み - Qiita](https://qiita.com/takefumiyoshii/items/cc3efe47490fc37ce642)