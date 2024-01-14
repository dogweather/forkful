---
title:                "Javascript: ディレクトリが存在するかどうかを確認する方法"
simple_title:         "ディレクトリが存在するかどうかを確認する方法"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

なぜディレクトリが存在するかどうかをチェックする必要があるのでしょうか？それは、コードを実行する前に事前に条件をチェックすることで、エラーを回避するためです。

## 作り方

まず、存在をチェックしたいディレクトリのパスを指定します。その後、`fs.exists`メソッドを使用してディレクトリが存在するかどうかを確認します。以下のコードブロックを参考にしてください。

```Javascript
const fs = require('fs'); // fsモジュールを読み込む

const dirPath = './myFolder'; // チェックしたいディレクトリのパスを指定する

fs.exists(dirPath, (exists) => { // fs.existsメソッドを使用してディレクトリが存在するかどうかをチェックする
    if (exists) {
        console.log('ディレクトリが存在します。'); // ディレクトリが存在する場合はメッセージを表示する
    } else {
        console.log('ディレクトリが存在しません。'); // ディレクトリが存在しない場合はメッセージを表示する
    }
});
```

上記のコードを実行すると、指定したディレクトリが存在するかどうかに応じてメッセージが表示されます。

## ディープダイブ

`fs.exists`メソッドは非推奨のため、代わりに`fs.existsSync`メソッドを使用することが推奨されています。また、コールバック関数ではなく同期的に処理を行うことができるため、コードを読みやすくすることができます。

また、`fs.existsSync`メソッドは引数に渡したパスが実際にファイルかディレクトリかを区別することができます。これにより、ディレクトリの存在をチェックするだけでなく、ファイルの存在もチェックすることができます。

## 参考リンク

- [Node.js fsモジュール](https://nodejs.org/api/fs.html)
- [fs.exists()メソッドは実は非推奨だった](https://www.adammaras.com/blog/nodejs-fs-exists-is-deprecated)
- [fs.existsSync()メソッドの使い方](https://www.javadrive.jp/javascript/fs/index8.html)