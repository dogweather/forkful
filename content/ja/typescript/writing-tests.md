---
title:                "TypeScript: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/writing-tests.md"
---

{{< edit_this_page >}}

# なぜテストを書くのか？

テストを書くのは、プログラムをより信頼性の高いものにするためです。テストを書くことで、バグを早期発見し、修正することができます。また、将来的な変更や追加が行われた際に、テストがあることで機能の互換性や動作の保証ができます。

## テストの書き方

テストを書くには、まずテストするコードが書かれたファイルと同じ場所に「spec」というフォルダを作成します。その中に、テストコードを書くためのファイルを作成します。テストコードは「.spec.ts」の拡張子をつけることが多いです。

例えば、以下のような関数をテストする場合、テストコードを書くことになります。

```TypeScript
// 以下、テストする関数を含むファイルのコード
export function addNumbers(num1, num2) {
  return num1 + num2;
}
```

```TypeScript
// テストコード
import { addNumbers } from './file-to-test';

describe('addNumbers', () => {
  it('should add two numbers correctly', () => {
    // テストする関数の実行
    const result = addNumbers(2, 3);

    // 期待する出力と実際の出力を比較
    expect(result).toEqual(5);
  });
});
```

上記のように、テストコードでは「describe」と「it」を使ってテストスイートを作成します。「describe」はテストのグループを作るために使用し、「it」は個々のテストケースを表します。テストコードでは、「import」でテストするファイルから関数を読み込み、テストケースの中で関数を実行し、期待する出力と実際の出力を比較することでテストを行います。

## テストの深堀り

テストの深堀りでは、テストの種類や良いテストコードの書き方など、より詳細にテストについて解説します。テストは単体テストや結合テストなど、様々な種類があり、それぞれのテストがどのような目的で行われるのかを理解することが重要です。また、テストのコードは可読性が高く、バグがないように注意深く書く必要があります。

## 他の参考リンク

- [TypeScript公式ドキュメント: テストを行う](https://www.typescriptlang.org/docs/handbook/testing.html)
- [Qiita: TypeScriptでテストを書く方法](https://qiita.com/ochaochaocha3/items/6777d97e9ac39c1fa988)
- [TDD Bootcamp: TypeScriptでテスト駆動開発をするための基本的な手法](https://tddbc.connpass.com/event/146442/)