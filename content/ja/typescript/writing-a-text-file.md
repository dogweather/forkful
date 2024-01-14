---
title:    "TypeScript: テキストファイルの作成"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# なぜテキストファイルを書きたいのか

テキストファイルを書くことは、プログラマーにとって非常に重要なスキルです。テキストファイルには、コードやデータを保存するために使用することができます。また、ドキュメントやメモを記録するためにも使われます。テキストファイルを書くことで、プログラマーは効率的に作業を行うことができます。

## 書き方

まずは、TypeScriptの `fs`モジュールを使用して、ファイルを開いて書き込む方法を見ていきましょう。

```
TypeScript
import * as fs from 'fs';

// 書き込むファイルを作成
fs.writeFile('hello.txt', 'こんにちは、世界！', (err) => {
  if (err) throw err;
});
```

上記のコードを実行すると、ファイルが作成され、中には「こんにちは、世界！」というテキストが書き込まれます。

次に、既存のテキストファイルに追記する方法を見てみましょう。

```
TypeScript
import * as fs from 'fs';

// ファイルを開く
fs.open('hello.txt', 'a', (err, fd) => {
  if (err) throw err;

  // ファイルに追記
  fs.appendFile(fd, '追記されたテキスト', (err) => {
    fs.close(fd, (err) => {
      if (err) throw err;
    });
  });
});
```

上記のコードを実行すると、既存の「hello.txt」ファイルに「追記されたテキスト」という文字列が追加されます。

## ディープダイブ

テキストファイルに書き込む際には、複数のファイルを扱うこともあります。その際には、読み込みや書き込みの順序に注意する必要があります。また、ファイル操作を行う前には、ファイルが存在するかどうかを確認することも重要です。

上記の例では、エラーハンドリングを行っていませんが、実際のアプリケーションではエラー処理をしっかり行うようにしましょう。

# 参考リンク

- [Node.jsのfsモジュールドキュメント](https://nodejs.org/api/fs.html)
- [TypeScriptの型安全なファイルストリーム操作について](https://blog.shnewto.com/typescript-typed-file-stream/)
- [あなたもTypeScriptでテキストファイルを書いてみよう！](https://qiita.com/babie/items/a6c7338f070c8976a297)