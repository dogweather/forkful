---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:04.066524-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.762998-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ?
TypeScriptでテストを書くとは、コードの機能性と正確性を検証する自動化されたスクリプトを作成することです。プログラマーは、信頼性を確保し、バグを迅速に捕捉し、保守可能なコード成長を容易にするためにこれを行います。なぜなら、TypeScriptの静的型付けはJavaScriptテストに予測可能性のレベルを追加するからです。

## 方法:
TypeScriptは、ほとんどのJavaScriptテストフレームワークと調和して動作します。デモンストレーションの目的で、TypeScriptプロジェクトのゼロ設定セットアップのため、人気のあるテストフレームワークであるJestを使用します。

まず、Jestと必要なTypeScriptタイプがインストールされていることを確認します：

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

次に、`jest.config.js`を変更するか、新しく作成してJestがTypeScriptで動作するように設定します：

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

さて、簡単な関数とそれに対するテストを書きましょう。次の関数を含む`sum.ts`ファイルを考えてみましょう：

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

`sum.test.ts`というテストファイルを作成します：

```typescript
// sum.test.ts
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

テストを実行するには：

```bash
npx jest
```

テストが通ったことを示すサンプル出力は次のようになります：

```plaintext
 PASS  ./sum.test.ts
  ✓ adds 1 + 2 to equal 3 (2 ms)
```

非同期コードの場合、Jestは`async/await`で対応します。非同期の`fetchData`関数を持っているとします：

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

非同期関数を使用したテスト：

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('fetches data successfully', async () => {
  expect(await fetchData()).toBe('data');
});
```

テストを実行すると、Jestはプロミスが解決するのを待ち、非同期操作を正確にテストします。

効果的なテストには、期待通りにTypeScriptコードが動作することを確認するために、異なるシナリオやエッジケースを含む複数のテストを書くことが含まれます。
