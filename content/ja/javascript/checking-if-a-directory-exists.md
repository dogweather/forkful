---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "C#: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかどうかを確認するのは、その名の通り、コンピュータ上で特定のディレクトリが存在するかどうかを確認する処理です。これは、ファイルの読み書きやディレクトリの操作を行う前に、予期しないエラーを防ぐために必要になります。

## どうやって：

以下に具体的なコードと出力の例を示します。

```Javascript
const fs = require('fs');

// ディレクトリの存在を確認
if (fs.existsSync('/path/to/directory')) {
    console.log('ディレクトリが存在します');
} else {
    console.log('ディレクトリが存在しません');
}
```

出力が下記のとおりです。

```
ディレクトリが存在します.
```
または

```
ディレクトリが存在しません.
```

## 深堀り

歴史的な文脈では、以前のNode.jsのバージョンでは、`fs.exists`や`fs.existsSync`が非推奨とされていました。しかし、現在のバージョンでは非推奨ではありません。

代替案としては、`fs.access`や`fs.accessSync`を使う方法があります。ただし、これらはファイル/ディレクトリにアクセスできるかどうかを確認するもので、存在するかどうかだけを確認する場合には`fs.existsSync`の方がシンプルです。

実装の詳細については、`fs.existsSync`関数は指定したパスが存在するかどうかを同期的に（ブロックされる）確認します。結果はブール値で返されます。

## 関連資料

- [`fs` APIドキュメンテーション](https://nodejs.org/api/fs.html)
- [Node.jsでのファイル・ディレクトリの確認方法](https://www.metachris.com/2015/12/different-ways-to-check-if-a-file-exists-in-node-js/)
- [ディレクトリ操作関連のnpmパッケージ](https://www.npmjs.com/package/mkdirp)