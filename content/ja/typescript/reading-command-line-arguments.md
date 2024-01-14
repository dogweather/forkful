---
title:                "TypeScript: コンピュータープログラミングを読む：コマンドライン引数"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

今回のポストでは、コマンドライン引数を読み取ることの重要性について説明します。コマンドライン引数を読み取る方法と、その深い理解についても具体的なコード例とともに紹介します。

## 読者にお薦めの理由

コマンドライン引数を読み取ることは、プログラミングでよく使用される必須のスキルです。最初は難しいかもしれませんが、コマンドライン引数を読み取ることで、プログラムをより柔軟に動作させることができます。また、コマンドライン引数を使用することで、ユーザーにより多くのオプションを提供することができます。

## コマンドライン引数の読み取り方

まずは、```process.argv```オブジェクトを使用して、コマンドライン引数を読み取ることができます。以下のコード例を参考にしてください。

```TypeScript
// 実行時に引数を指定する
// node index.ts arg1 arg2 arg3

console.log(process.argv[0]); // 'node'
console.log(process.argv[1]); // 'index.ts'
console.log(process.argv[2]); // 'arg1'
console.log(process.argv[3]); // 'arg2'
console.log(process.argv[4]); // 'arg3'
```

## 深堀り

メインファイルが```index.ts```の場合、コマンドライン引数の最初の要素は```index.ts```そのものになります。したがって、ユーザーから指定された引数は、実際の引数のインデックスが2から始まることに注意してください。また、```process.argv```では、文字列として引数が返されるので、必要に応じてデータ型の変換を行う必要があります。

## お勧めのリンク

この記事で紹介した```process.argv```以外にも、コマンドライン引数の取得方法は様々あります。以下のリンクを参考に、さらに詳しく学習してみてください。

- [Node.jsのコマンドライン引数の読み取り方](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [TypeScriptでのコマンドライン引数の取得方法](https://www.typescriptlang.org/docs/handbook/utility-types.html#unpacked)