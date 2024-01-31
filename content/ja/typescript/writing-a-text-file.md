---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

category:             "TypeScript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
テキストファイルを書くことは、データを永続的に保存することです。プログラマはデータ交換、設定の保存、ログの生成にこれを行います。

## How to: (やり方)
```TypeScript
import { writeFile } from 'fs';

// テキストを保存したい内容
const content: string = "こんにちは、TypeScript!";

// ファイルに内容を書き込む関数
const writeTextToFile = (filePath: string, text: string): void => {
  writeFile(filePath, text, (err) => {
    if (err) {
      console.error('エラー発生:', err);
      return;
    }
    console.log(`ファイル ${filePath} に書き込み完了`);
  });
};

// 関数を使ってみる
writeTextToFile('./hello.txt', content);
```
サンプル出力:
```
ファイル ./hello.txt に書き込み完了
```

## Deep Dive (深掘り)
初期のコンピュータでは、パンチカードやテープを使ってデータを保存していました。今日では、テキストファイルはJSONやXMLなどの形式で設定やデータ交換に使われます。Node.jsの`fs`モジュールは非同期I/Oを提供し、パフォーマンスの妨げにならないやり方でファイルシステムにアクセスできます。`writeFile`関数はファイルがない場合は作成し、ある場合は上書きします。同期的に書き込むには`writeFileSync`を使用できますが、ブロッキングが問題になる可能性があります。

## See Also (関連情報)
- Node.js fsモジュールドキュメント: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- TypeScript公式ドキュメント: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- ファイルシステムに関するMDN Web Docs: [https://developer.mozilla.org/ja/docs/Web/API/File_System_Access_API](https://developer.mozilla.org/ja/docs/Web/API/File_System_Access_API)
- JSONについての情報: [https://www.json.org/json-ja.html](https://www.json.org/json-ja.html)
- XMLに関する情報: [https://www.w3.org/XML/](https://www.w3.org/XML/)
