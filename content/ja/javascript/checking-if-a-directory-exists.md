---
title:    "Javascript: ディレクトリが存在するかどうかのチェック"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックすることの重要性は、プログラミングにおけるエラー処理にあります。ディレクトリが存在しない場合、それに対する適切な処理を行わなければなりません。そのため、正確にディレクトリが存在するかを確認することは、プログラミングにおいて不可欠です。

## 方法

ディレクトリの存在をチェックするには、Node.jsのfsモジュールの`existsSync()`メソッドを使用します。以下のコード例をご覧ください。

```Javascript
const fs = require("fs");

// 指定したパスのディレクトリが存在するかどうかをチェックする
if(fs.existsSync("/path/to/directory")){
  console.log("ディレクトリが存在します。");
} else {
  console.log("ディレクトリは存在しません。");
}
```

上記のコードでは、`existsSync()`メソッドがディレクトリが存在するかどうかを返します。存在する場合は`true`、存在しない場合は`false`です。また、指定したパスには相対パスや絶対パスのどちらも使用することができます。

## 詳細説明

`existsSync()`メソッドは同期的なメソッドであり、ファイルシステムへのアクセスを行います。そのため、他の処理をブロックする可能性があることに注意してください。また、Node.js以外の環境（ブラウザなど）では使用できません。

さらに、`existsSync()`メソッドはディレクトリの存在を確認するだけでなく、ファイルの存在も確認することができます。その場合は、引数にファイルのパスを指定してください。

## 参考リンク

- [Node.js fsモジュール](https://nodejs.org/api/fs.html)
- [existsSync()メソッドのドキュメント](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [ファイルパスについての詳細情報](https://nodejs.org/api/path.html#path_path_freepath_path)

## 関連リンク

- [Node.jsドキュメント](https://nodejs.org/ja/docs/)
- [フロントエンド開発の基本](https://digitalidentity.co.jp/blog/frontend-basics/)