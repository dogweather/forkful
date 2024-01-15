---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Javascript: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##なぜ
ディレクトリが存在するかどうかを確認する理由は何でしょうか？それはプログラムの実行中に必要なファイルやフォルダーが存在しているかどうかを確認する必要があるためです。例えば、ファイルを操作する場合には、事前にその存在を確認したうえで操作する必要があります。 

##方法 
```Javascript 
// サンプルディレクトリパス 
const directoryPath = "path/to/directory";

// fsモジュールの読み込み 
const fs = require("fs");

// ディレクトリが存在するかどうかを確認する関数 
const checkDirectoryExists = (path) => {
  fs.access(path, fs.F_OK, (err) => {
    if (err) {
      console.log(`${path} does not exist`);
    } else {
      console.log(`${path} exists`);
    }
  });
};

// 関数の呼び出し 
checkDirectoryExists(directoryPath);

// 出力例
// "path/to/directory exists"
``` 

##深堀り 
ファイルやディレクトリの存在を確認するには、fsモジュールの"fs.access()"メソッドを使用します。第一引数にチェックするパスを指定し、第二引数にはチェックするアクセス権限を指定します。このメソッドはコールバック関数を受け取り、エラーが発生した場合にはエラーオブジェクトが渡されます。エラーがない場合にはファイルやディレクトリが存在することを示すメッセージが表示されます。 

##参考リンク 
- [Node.js公式ドキュメント: fs.access()](https://nodejs.org/api/fs.html#fs_fs_access_path_mode_callback) 
- [Qiita: Node.jsでファイル、ディレクトリの存在をチェックする](https://qiita.com/r-funabashi/items/946512e2f22227e30a3f) 

##関連リンク 
- [Node.js公式ドキュメント: fsモジュール](https://nodejs.org/api/fs.html) 
- [Wikipedia: ファイルシステム](https://ja.wikipedia.org/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%82%B7%E3%82%B9%E3%83%86%E3%83%A0)