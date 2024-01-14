---
title:                "TypeScript: デバッグ出力の印刷"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力をプリントすることで、失敗したテストや問題の特定に役立ちます。また、コードの特定の部分の実行を追跡することもできます。

## 方法

デバッグ出力をプリントするには、`console.log()`メソッドを使用します。例えば、以下のように使用することができます。

```TypeScript
const name = "John";
console.log("Hello, " + name);
```

このコードを実行すると、コンソールに「Hello, John」という出力が表示されます。

## 深堀り

デバッグ出力をプリントすることで、コードの実行や処理の流れをより詳細に把握することができます。また、`console.log()`メソッドはオブジェクトや配列などの複雑なデータ型も表示することができるため、デバッグの際に有用です。

## 参考リンク

![See Also](https://i.imgur.com/CmDbfZq.png)

- [TypeScript 公式ドキュメントのconsole.log()メソッドの説明](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html)
- [MDN web docsのconsole.log()メソッドの説明](https://developer.mozilla.org/ja/docs/Web/API/Console/log)
- [TypeScriptでデバッグ出力をプリントする方法](https://www.codingdojo.com/blog/how-to-debug-typescript-code/)