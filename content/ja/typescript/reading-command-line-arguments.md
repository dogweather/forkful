---
title:                "コンピューター・プログラミングの記事タイトル：コマンドライン引数の読み取り"
html_title:           "TypeScript: コンピューター・プログラミングの記事タイトル：コマンドライン引数の読み取り"
simple_title:         "コンピューター・プログラミングの記事タイトル：コマンドライン引数の読み取り"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ？

コマンドライン引数を読み取ることは、一見難しく感じられるかもしれませんが、実際には非常に役に立つスキルです。たとえば、ユーザーから動的な入力を受け取る必要がある場合や、複数のバージョンのプログラムを実行する際に役立ちます。

## 使い方

コマンドライン引数を読み取るには、`process.argv`メソッドを使用します。これはNode.jsで提供されるグローバルオブジェクトです。具体的なコーディング例は以下の通りです。

```TypeScript
// 実行コマンド：node myProgram.ts firstArg secondArg

const args = process.argv.slice(2);
console.log(args[0]); // firstArg
console.log(args[1]); // secondArg
```

このように、`process.argv`メソッドを使用して引数を読み取ることができます。

## もっと深く掘り下げる

コマンドライン引数にはさまざまなオプションがあり、それぞれの役割を理解することは重要です。たとえば、`process.argv[0]`は実行中のプログラムのパスを示し、`process.argv[1]`は実行中のスクリプトのパスを示します。また、スイッチと呼ばれる特殊な引数があり、値の有無に基づいてプログラムの動作を変更することができます。詳細な情報は公式ドキュメントを参照してください。

## 同じ為参照

- [Node.jsの公式ドキュメント](https://nodejs.org/docs/latest/api/process.html#process_process_argv)