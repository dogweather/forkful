---
title:                "TypeScript: ディレクトリが存在するかどうかを確認する"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ディレクトリが存在するかどうかを確認する必要がある理由は、プログラムの実行中にファイルを読み書きしたい場合や、特定の条件で特定の処理を実行する必要がある場合、あるいはファイルのバリデーションを行う必要がある場合です。

## 方法
ディレクトリが存在するかどうかを確認するには、TypeScriptの組み込み関数である`fs.existsSync()`を使用します。以下の例を参考にしてください。

```TypeScript
import * as fs from "fs";
const directoryExists = fs.existsSync("path/to/directory");

if(directoryExists) {
  console.log("ディレクトリは存在します。");
} else {
  console.log("ディレクトリは存在しません。");
}
```

もしディレクトリが存在する場合、`directoryExists`変数は`true`が返されます。存在しない場合は`false`が返されます。また、`fs.existsSync()`は同期的な関数なので、プログラムの実行を一時停止することなく即座に結果を取得できます。

## 深堀り
`fs.existsSync()`は、ファイルシステムファイルのチェックを行う際によく使用される関数です。ディレクトリの存在を確認するために使用する以外にも、ファイルが存在するか、ファイルのメタデータを取得する際にも使用することができます。

また、`fs.existsSync()`はNode.jsのファイルシステムモジュールであるfsモジュールに属していますが、他のモジュールであるpathモジュールと組み合わせて使用することもできます。例えば、`fs.existsSync()`のパス引数にpathモジュールの`path.resolve()`関数を使用することで、相対パスの問題を避けることができます。

## それを参照してください
[Node.js公式ドキュメント：fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)

[Node.js公式ドキュメント：path.resolve()](https://nodejs.org/api/path.html#path_path_resolve_paths)

[TypeScript公式ドキュメント：fs.existsSync()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#fs-exists)

[Qiita：ディレクトリが存在するかどうかを確認する](https://qiita.com/hirosy1/items/40a67faee011d4c72630)