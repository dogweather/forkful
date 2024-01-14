---
title:    "TypeScript: 「標準エラーに書き込む」"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## なぜ

標準エラー出力を書くことは、エラーの追跡や修正、デバッグに非常に重要です。また、プログラミングのスキルを向上させるためにも役立ちます。

## 方法

まず、TypeScriptファイルを作成します。その後、`console.error()`メソッドを使用して、エラーを出力することができます。

```TypeScript
const error = "これはエラーメッセージです。";
console.error(error);
```

出力は以下のようになります。

```
> これはエラーメッセージです。
```

また、条件によってエラーを出力することもできます。例えば、以下のように変数が数値ではない場合にエラーを出力することができます。

```TypeScript
const value = "テキスト";
if (typeof value !== "number") {
  console.error("数値ではありません。");
}
```

出力は以下のようになります。

```
> 数値ではありません。
```

## 深堀り

標準エラー出力を使う際に注意すべき点が2つあります。1つ目は、標準出力と同じく、エラー出力もリダイレクトしてファイルに書き込むことができる点です。例えば、以下のコマンドを実行すると、エラー出力が`error.log`というファイルに書き込まれます。

```bash
$ node index.ts 2> error.log
```

2つ目は、`console.error()`メソッドは任意の数の引数を取ることができる点です。そのため、複数のエラーを一度に出力することができます。

```TypeScript
console.error("エラー1", "エラー2", "エラー3");
```

出力は以下のようになります。

```
> エラー1 エラー2 エラー3
```

## もっと詳しく知る

標準エラー出力については、さらに詳しい情報を以下のリンクから参照することができます。

- [Node.js公式ドキュメント](https://nodejs.org/api/process.html#process_writing_to_stderr)
- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)

## 関連リンク

- [TypeScript公式ドキュメント](https://www.typescriptlang.org/docs/)
- [プログラミングにおけるエラー処理の重要性とは？](https://www.sourcetree.jp/introduction/programing_importance_error.html)
- [ファイル出力を行う方法](https://dev.classmethod.jp/articles/coder-tips-node-typescript-stderr/)