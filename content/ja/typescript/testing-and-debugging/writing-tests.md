---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:04.066524-07:00
description: "\u65B9\u6CD5: TypeScript\u306F\u3001\u307B\u3068\u3093\u3069\u306EJavaScript\u30C6\
  \u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u3068\u8ABF\u548C\u3057\u3066\
  \u52D5\u4F5C\u3057\u307E\u3059\u3002\u30C7\u30E2\u30F3\u30B9\u30C8\u30EC\u30FC\u30B7\
  \u30E7\u30F3\u306E\u76EE\u7684\u3067\u3001TypeScript\u30D7\u30ED\u30B8\u30A7\u30AF\
  \u30C8\u306E\u30BC\u30ED\u8A2D\u5B9A\u30BB\u30C3\u30C8\u30A2\u30C3\u30D7\u306E\u305F\
  \u3081\u3001\u4EBA\u6C17\u306E\u3042\u308B\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\
  \u30EF\u30FC\u30AF\u3067\u3042\u308BJest\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\
  \u2026"
lastmod: '2024-04-05T22:37:50.059810-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u306F\u3001\u307B\u3068\u3093\u3069\u306EJavaScript\u30C6\u30B9\
  \u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u3068\u8ABF\u548C\u3057\u3066\u52D5\
  \u4F5C\u3057\u307E\u3059\u3002\u30C7\u30E2\u30F3\u30B9\u30C8\u30EC\u30FC\u30B7\u30E7\
  \u30F3\u306E\u76EE\u7684\u3067\u3001TypeScript\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\
  \u306E\u30BC\u30ED\u8A2D\u5B9A\u30BB\u30C3\u30C8\u30A2\u30C3\u30D7\u306E\u305F\u3081\
  \u3001\u4EBA\u6C17\u306E\u3042\u308B\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\
  \u30FC\u30AF\u3067\u3042\u308BJest\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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
