---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:58:53.360173-07:00
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリが存在するかチェックするとは、ファイルシステム上で特定のディレクトリが存在するかどうかを確認することです。これをプログラマーが行うのは、ファイルの読み書きやディレクトリの作成前にエラーを避けるためです。

## How to: (方法)
```TypeScript
import * as fs from 'fs';
import * as path from 'path';

// 非同期関数を用いた例
async function checkDirectoryExists(dirPath: string): Promise<void> {
  try {
    await fs.promises.access(dirPath, fs.constants.F_OK);
    console.log(`Directory exists: ${dirPath}`);
  } catch {
    console.error(`Directory does not exist: ${dirPath}`);
  }
}

// 使用例
const myDir = path.join(__dirname, 'myDirectory');
checkDirectoryExists(myDir);
```

サンプル出力:
```
Directory exists: /your/current/directory/myDirectory
// または
Directory does not exist: /your/current/directory/myDirectory
```

## Deep Dive (詳細な解説)
ディレクトリの存在確認には主に`fs`モジュールが使われます。 `fs.stat` や `fs.access` はノードの初期から利用可能ですが、Node.js v10.0.0からはPromiseベースのAPIが追加され、`fs.promises`オブジェクトを通じて非同期関数を使うことが推奨されています。同期的なチェックを行いたい場合は、`fs.existsSync`関数もありますが、これはブロッキングであり、大規模なアプリケーションでは非同期版が一般に推奨されています。

## See Also (関連情報)
- Node.js FileSystem API: https://nodejs.org/api/fs.html
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
- Asynchronous Programming (Patterns and Best Practices): https://nodejs.dev/learn/modern-asynchronous-javascript-using-promises-async-await-and-callbacks
