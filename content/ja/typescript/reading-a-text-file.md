---
title:    "TypeScript: 「テキストファイルの読み込み」"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むことのメリットは何でしょうか？テキストファイルは、プログラム内でデータを保存する一般的な方法であり、後から使えるようにします。また、複数のユーザーが同じファイルを共有することもできます。

## 方法

まず、TypeScriptでファイルを読み込むには、`fs`モジュールを使用する必要があります。下記のコードブロックを参考にしてください。

```TypeScript
import fs from 'fs';

//ファイルを同期的に読み込む
const fileContent = fs.readFileSync('sample.txt', 'utf-8');
console.log(fileContent);

//ファイルを非同期的に読み込む
fs.readFile('sample.txt', 'utf-8', (err, data) => {
    if (err) throw err;
    console.log(data);
});
```

出力結果は、読み込んだファイルの内容がコンソール上に表示されます。

## ディープダイブ

テキストファイルを読み込む際に注意するポイントがあります。まず、ファイルを読み込む前に`fs.existsSync()`メソッドを使用して、ファイルが存在するかを確認する必要があります。また、ファイルを読み込む際のエンコーディング形式も指定することが重要です。`utf-8`以外のエンコーディング形式を使用する場合は、読み込んだデータを`Buffer`オブジェクトとして扱う必要があります。詳細な情報は、公式ドキュメントを参照してください。

## 参考リンク

- [Node.js - ファイルシステム](https://nodejs.org/api/fs.html)
- [TypeScript Handbook - ファイル](https://www.typescriptlang.org/docs/handbook/working-with-files.html)
- [Node.js - Repl.it](https://repl.it/languages/nodejs) (オンライン上でコードを試すことができます)
- [Node.js - 日本語ドキュメント](https://nodejs.jp/nodejs.org_ja/docs/js/global.html#fs_fs_readfilesync_path_options) (日本語で読める公式ドキュメント) 

## 他の参考リンク

- [Node.jsを始めよう - ファイル操作入門](https://dotinstall.com/lessons/basic_nodejs_v2/72801)
- [Learn Node.js in 1 Hour](https://www.youtube.com/watch?v=TlB_eWDSMt4) (1時間でNode.jsを学ぶことができる動画チュートリアル)