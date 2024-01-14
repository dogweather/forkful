---
title:                "Javascript: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ？

テキストファイルを読むことの重要性を理解するには、プログラミングにおける基本的なスキルであると認識することが重要です。テキストファイルを読むことで、データを取り込んだり、処理したりすることができるようになります。

## テキストファイルの読み方

```Javascript
// ファイルシステムモジュールを使用する
const fs = require('fs');

// テキストファイルを読み込む
fs.readFile('sample.txt', 'utf8', (err, data) => {
    // ファイルの読み込みに成功した場合
    if (!err) {
        // データをコンソールに出力する
        console.log(data);
    } else {
        // ファイルの読み込みに失敗した場合
        console.log(err);
    }
});
```

### 出力結果
```
こんにちは、世界！
Hello, World!
```

## 詳しく調べる

テキストファイルの読み込みでは、`fs.readFile()`メソッドを使用しています。このメソッドは、非同期的にファイルを読み込むため、コールバック関数を使用して処理を行います。第一引数には読み込むファイルのパスを指定し、第二引数には文字コードを指定します。そしてコールバック関数の第一引数にはエラーオブジェクトが渡され、第二引数には読み込んだデータが渡されます。

## 参考リンク

- [Node.js公式ドキュメント](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [TechAcademy | テキストファイルを読み書きする方法](https://techacademy.jp/magazine/33202)
- [Qiita | Node.jsでテキストファイルの読み込みをしてみよう](https://qiita.com/moririn772/items/2852b5300e400a0a6ff8)