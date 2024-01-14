---
title:    "TypeScript: ディレクトリが存在するかどうかをチェックする"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認することの利点は、コードを書く際に特定のフォルダーが存在しているかどうかを確実にするためです。これにより、プログラムがエラーをスローしたり、予期せぬ動作をすることを防ぐことができます。

## 使い方
あなたはTypeScriptを使ってコードを書くことがあるかもしれません。そうすると、ディレクトリが存在するかどうかを確認する必要があるかもしれません。その場合、以下のコードを使用することができます。

```TypeScript
const fs = require('fs');

const directory = './folder';

// ディレクトリが存在するかどうかを確認する
if (fs.existsSync(directory)) {
  console.log('ディレクトリが存在します。');
} else {
  console.log('ディレクトリは存在しません。');
}
```

上記のコードでは、`fs`モジュールを使用して指定されたフォルダーが存在するかどうかを確認しています。もし存在すれば、「ディレクトリが存在します。」というメッセージが表示されます。もし存在しなければ、「ディレクトリは存在しません。」というメッセージが表示されます。

## 詳細について
ディレクトリの存在を確認するために、`fs.existsSync()`メソッドを使用します。このメソッドは、引数として指定されたパスが存在するかどうかを確認し、ブール値で返します。もし存在すれば`true`を、存在しなければ`false`を返します。

`fs.existsSync()`メソッドの代わりに、`fs.stat()`メソッドを使用することもできます。これは、ファイルやフォルダーの情報を取得するために使用されます。もし指定されたパスが存在しなければ、`ENOENT`というエラーがスローされます。

## それでは
ディレクトリが存在するかどうかを確認する方法を学びました。これを使用することで、コードを実行する前にフォルダーが存在するかどうかをチェックすることができます。これにより、予期せぬエラーを防ぐことができます。

## 関連リンク
- [fs.existsSync() ドキュメント](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [fs.stat() ドキュメント](https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback)