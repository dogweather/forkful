---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Go: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリ存在チェックは、指定したディレクトリが現在存在しているかどうかを確認することです。プログラマーは、操作の前にディレクトリが存在することを確認して、存在しない場合にはディレクトリを作成またはエラーを報告するためにこれを行います。

## 方法：

以下のTypeScriptソースコードは、ディレクトリが存在するかどうかをチェックする方法の一つを示しています:

```TypeScript
import fs from 'fs';

function directoryExists(path: string): boolean {
    return fs.existsSync(path);
}

console.log(directoryExists('./myFolder'));  // true or false
```

このプログラムはfsモジュールの`existsSync`関数を使用してディレクトリが存在するかどうかをチェックします。`./myFolder`という名前のディレクトリが存在する場合は`true`を、存在しない場合は`false`を出力します。

## 深掘り：

TypeScriptの初期バージョンでは、このような機能は存在せず、Node.jsのfsモジュールの`existsSync`関数を使用するデフォルトのアプローチが必要でした。

他の代替方法として、非同期バージョンの`exists`関数を利用することもできますが、非同期性が避けられない場合にのみ推奨されます。これは、`existsSync`がブロッキングオペレーションを提供するのに対し、`exists`はノンブロッキングオペレーションを提供するためです。

なお、`exists`関数は現在非推奨とされており、代わりに`access`関数または`stat`関数を使用することが推奨されています。これらの関数は、より詳細な情報を提供し、Promiseベースの非同期APIも提供しています。

## 参考文献：

1. Node.js fsモジュールドキュメント：[https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
2. `fs.existsSync`関数についての詳細：[https://nodejs.org/api/fs.html#fs_fs_existssync_path](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
3. `fs.access`関数についての詳細：[https://nodejs.org/api/fs.html#fs_fs_access_path_mode_callback](https://nodejs.org/api/fs.html#fs_fs_access_path_mode_callback)
4. `fs.stat`関数についての詳細：[https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback](https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback)