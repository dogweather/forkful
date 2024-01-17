---
title:                "ディレクトリの存在を確認する"
html_title:           "Gleam: ディレクトリの存在を確認する"
simple_title:         "ディレクトリの存在を確認する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

---

Gleam: ディレクトリの存在を確認する

## 何か? それは何ですか？

ディレクトリの存在を確認するとは、コンピュータ上にある特定のフォルダーが実際に存在するかどうかを判断することです。プログラマーは、プログラムが正しく実行されるために、必要なディレクトリが存在するかどうかを確認する必要があります。

## 方法：

Gleamでは、```fs.exists```関数を使用してディレクトリの存在を確認することができます。まず、必要なモジュールをインポートします。

```Gleam
import fs

```

次に、```fs.exists```を使用してディレクトリが存在するかどうかをチェックします。チェックするディレクトリのパスを指定し、結果をBool値として返します。

```Gleam
let dir = "/Users/johndoe/documents"
let exists = fs.exists(dir)

/* 出力
true
*/
```

もしくは、存在しないディレクトリをチェックした場合には、false値を返します。

```Gleam
let dir = "/Users/johndoe/pictures"
let exists = fs.exists(dir)

/* 出力
false
*/
```

## 詳細を見る

ディレクトリの存在を確認する必要性は、プログラミングの歴史に関わるものです。昔のコンピュータでは、エラーメッセージは不親切だったため、プログラム実行時に必要なフォルダーがないとエラーが発生していました。そのため、事前にディレクトリの存在を確認することが重要になりました。

Gleamでは、```fs.exists```以外にも、ファイルシステムに関する機能を提供しているため、ディレクトリの存在確認だけでなく、他のファイル操作も可能です。また、別の方法として、コマンドラインでディレクトリをチェックすることもできます。

## 関連情報を見る

- ```fs``` モジュールの公式ドキュメント: https://gleam.run/modules/fs.html
- より詳細なファイル操作方法を学ぶ: https://en.wikibooks.org/wiki/Gleam/FileSystem/File_I/O
- コマンドラインでディレクトリをチェックする方法: https://www.geeksforgeeks.org/check-if-a-directory-exists-in-a-given-path-in-python/