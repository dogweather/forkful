---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Javascript: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Introducing the concept of checking if a directory exists in Javascript.

## What & Why?

ディレクトリが存在するかどうかをチェックすることは、ファイルシステム内の特定のディレクトリが現在存在するかどうかを確認することです。プログラマーは、コード内で特定のディレクトリにアクセスする前に、ディレクトリの存在を確認することができます。

## How to:

```Javascript
// ディレクトリの存在をチェックする方法
const fs = require('fs'); // ファイルシステムモジュールを読み込む
const directoryPath = '/Users/username/Documents'; // チェックするディレクトリのパス
const isDirectoryExist = fs.existsSync(directoryPath); // fs.existsSync()関数でディレクトリの存在を確認する
console.log(isDirectoryExist); // ディレクトリが存在する場合、trueを出力
```

## Deep Dive

ディレクトリの存在を確認する必要性は、コンピューターのファイルシステムにおける基本的な概念です。ファイルシステム内では、ディレクトリはファイルを整理するための重要な役割を果たします。プログラマーは、ファイルの読み書きを行う前に、ディレクトリが存在するかどうかを確認することで、エラーを防ぐことができます。

ディレクトリの存在をチェックする方法は、実装によって異なります。上記のコードでは、Node.jsのfsモジュールを使用してディレクトリの存在を確認しましたが、他にもfs-extraやshelljsなどのモジュールを使用することもできます。

## See Also

- [fsモジュールのドキュメンテーション](https://nodejs.org/api/fs.html)
- [fs-extraのドキュメンテーション](https://github.com/jprichardson/node-fs-extra)
- [Youtubeで学ぶNode.jsファイルシステムチュートリアル](https://www.youtube.com/watch?v=SBKM8Qw7wRI)