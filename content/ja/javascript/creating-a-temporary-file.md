---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何それ？なぜしたい？
一時ファイルの作成は、プログラミングでしばしば使用される技術で、以下の2つの場面で役に立ちます。(1) 大量のデータを一時的に保存するとき、(2) 一時的な結果を共有するときです。

## どうやる？
Javascriptでは、`tmp`というライブラリを使用して一時ファイルを作成します。以下は簡単な例です。

```Javascript 
const tmp = require('tmp');

tmp.file((err, path, fd) => {
  if (err) throw err;

  console.log('Temporary file path: ', path);
});
```
上記のコードを実行すると、一時ファイルのパスがコンソールに表示されます。例えば、「Temporary file path: /tmp/abc123」といった形です。

## 深掘り

1. **歴史**
一時ファイルはUNIXシステムから始まり、プログラムが一時的なデータを保存し、後でアクセスするために使用されました。

2. **代替手段**
`fs`モジュールを使用して一時ファイルを作成することも可能ですが、`tmp`はより簡単で使い易いと言えます。

3. **実装詳細**
`tmp.file`関数はOSに依存せず、一時的な空のファイルを作成します。ファイルはプログラム終了時に自動的に削除されます。

## 参考になるリンク

* [tmpの公式文書](https://github.com/raszi/node-tmp): 一時ファイルの作成について更に詳しく知る。
* [Node.jsのfsモジュール](https://nodejs.org/api/fs.html): ファイルとフォルダの操作について詳しく学ぶ。