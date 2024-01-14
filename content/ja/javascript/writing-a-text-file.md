---
title:                "Javascript: テキストファイルの作成"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ
テキストファイルを書くことに興味を持った理由を紹介します。多くのプログラマーにとって、自分のコードやプログラムの実行結果を保存したり、他の人と共有したりするためにテキストファイルが必要になります。

## 方法
まず、テキストファイルを作成する方法から始めましょう。以下のようなコードを使って、テキストファイルを作成することができます。

```Javascript
// テキストファイルを作成する
var fs = require('fs');
fs.writeFile('myfile.txt', '私の最初のテキストファイルです。', function (err) {
  if (err) throw err;
  console.log('テキストファイルを作成しました。');
});
```

上記のコードを実行すると、`myfile.txt`という名前のテキストファイルが作成され、その中に`私の最初のテキストファイルです。`というテキストが書き込まれます。また、作成されたファイルの名前や書き込むテキストを変更することも可能です。

次に、既存のテキストファイルに追記する方法を紹介します。

```Javascript
// テキストファイルに追記する
var fs = require('fs');
fs.appendFile('myfile.txt', '新しいテキストを追加します。', function (err) {
  if (err) throw err;
  console.log('テキストファイルにテキストを追加しました。');
});
```

上記のコードを実行すると、既存の`myfile.txt`に`新しいテキストを追加します。`というテキストが追加されます。

## 深堀り
テキストファイルを作成して書き込む方法について、もう少し詳しく見ていきましょう。

まず、`fs.writeFile()`と`fs.appendFile()`の両方で使用している`fs`という変数ですが、これはNode.jsの`fs`モジュールを使用するために必要なものです。`require()`関数を使って`fs`モジュールのロードが必要です。

また、`fs.writeFile()`と`fs.appendFile()`では、コールバック関数を使用しています。コールバック関数とは、上記のコードの`function (err) { ... }`の部分のことで、ファイルの書き込みが完了した後に実行される関数です。書き込みに成功したかどうかは`err`を確認することで判断することができます。

さらに、テキストファイルを作成したり追記したりする場合、元のファイルに影響を与えずに新しいファイルを作成する必要があるかもしれません。そのような場合、`fs.createWriteStream()`を使用することができます。これは、新しいファイルを作成してそこに書き込むためのものです。

## 参考
- [Node.jsのドキュメント: ファイルシステム](https://nodejs.org/api/fs.html)
- [Node.jsでテキストファイルを操作する方法 (Qiita)](https://qiita.com/MakeNowJust/items/ed689f660b0ddeb62ef4)