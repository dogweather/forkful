---
title:                "「テストの書き方」"
html_title:           "TypeScript: 「テストの書き方」"
simple_title:         "「テストの書き方」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/writing-tests.md"
---

{{< edit_this_page >}}

# 何を & 何故？
テストを書くことは、プログラマーがコードを書いた後にそのコードが正しく動作するかどうかを確認するためのプロセスです。プログラムのバグを早期に発見し、品質を向上させるためにプログラマーはテストを実行します。

# 方法：
テストを書くのは簡単です。まず、各テストケースで期待される出力を定義します。次に、その出力を実際の出力と比較し、どちらも一致するかどうかを判断します。以下の例を参考にしてください。

```TypeScript
// マイナス記号を除去する関数
function removeMinus(num: number) {
  if (num < 0) {
    return num * -1;
  }
  else {
    return num;
  }
}

console.log(removeMinus(5));
// Output: 5

console.log(removeMinus(-10));
// Output: 10
```

# 詳細：
テストを書く慣習は、コンピュータサイエンスが発展してきた過程で生まれました。テストを書くことで、プログラムの品質を向上させ、バグを早期に発見することができます。テストを書く方法にはさまざまなアプローチがありますが、最も一般的なのはユニットテストと統合テストです。

他にも、人間が手動でコードを実行してテストする方法もありますが、これは効率的ではありません。そのため、プログラマーは自動化されたテストを書くことを推奨します。

実際にテストを書くプロセスは、テストフレームワークを使用して行われます。TypeScriptでは、JasmineやMochaなどの人気のあるフレームワークがあります。

# 関連リンク：
- テストフレームワークのドキュメント：[Jasmine](https://jasmine.github.io/)、[Mocha](https://mochajs.org/)
- TypeScriptの公式ドキュメント：[TypeScript](https://www.typescriptlang.org/)