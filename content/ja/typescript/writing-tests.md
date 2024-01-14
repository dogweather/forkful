---
title:    "TypeScript: Reply: テストを書く"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

プログラミングをする上で、テストは非常に重要な役割を担っています。テストを書くことで、コードが期待通りに動作するかどうかを確認し、バグを早期に発見することができます。これにより、品質の高いプログラムを作成することができます。

## 方法

テストを書く方法を学ぶ前に、まずはTypeScriptを使ったプログラミングの基本を覚えましょう。以下のコード例を参考にしてください。

```TypeScript
// 2つの数値を足し合わせる関数
function add(a: number, b: number): number {
  return a + b;
}

// テストケース
console.log(add(2,3)); // 出力結果：5
console.log(add(-1, 100)); // 出力結果：99
```

上記の例では、2つの数値を足し合わせる関数を作成し、それぞれのテストケースをConsole.logを使って出力しています。このように、テストを書くことでコードの動作を確認することができます。

## ディープダイブ

テストを書く際には、さまざまなタイプのテストがあります。単体テストや結合テストなど、それぞれの目的に応じて適切なテストを選択することが重要です。また、網羅的なテストを書くことで、コードのカバレッジを高めることができます。さらに、テストを自動化することで、プログラムの品質を保つことができます。

## See Also

- [TypeScript公式ドキュメント](https://www.typescriptlang.org/docs/)
- [Jestを使ったテストの自動化](https://jestjs.io/docs/getting-started)
- [テスト駆動開発についての教材](https://codeprep.jp/books/28)