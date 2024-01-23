---
title:                "テストの作成"
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストとは？なぜ書くの？)
テストはコードが正しく動作することを自動で保証する手法。バグを早期に発見し、機能が求める通りに動作しているか確認するために開発者が書く。

## How to: (実際にどうやって？)
Jestを使ってTypeScriptでのテストを書く基本的な方法を見てみよう。以下に単純な関数とそのテストの例を示す。

```TypeScript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}

// sum.test.ts
import { sum } from './sum';

test('sums two numbers correctly', () => {
  expect(sum(1, 2)).toBe(3);
});
```

コマンドラインで`jest`を実行すると以下のような出力が得られる。

```
PASS ./sum.test.ts
✓ sums two numbers correctly (5ms)
```

## Deep Dive (深掘り)
テストを書く文化はTDD (Test-Driven Development) から始まった。代替としてBDD (Behavior-Driven Development) などもある。TypeScriptでは型情報を利用してより正確なテストを書くことができる。例えば、型が合わなければコンパイルエラーになり、テスト前にバグを発見できる。

## See Also (関連情報)
- [Jest公式ドキュメント](https://jestjs.io/ja/)
- [Test-Driven Development (TDD) について](https://www.agilealliance.org/glossary/tdd/)
