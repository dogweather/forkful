---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:04.066524-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.762998-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3068\u306F\u3001\u30B3\
  \u30FC\u30C9\u306E\u6A5F\u80FD\u6027\u3068\u6B63\u78BA\u6027\u3092\u691C\u8A3C\u3059\
  \u308B\u81EA\u52D5\u5316\u3055\u308C\u305F\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\
  \u6210\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u4FE1\u983C\u6027\u3092\u78BA\u4FDD\u3057\u3001\u30D0\u30B0\u3092\u8FC5\
  \u901F\u306B\u6355\u6349\u3057\u3001\u4FDD\u5B88\u53EF\u80FD\u306A\u30B3\u30FC\u30C9\
  \u6210\u9577\u3092\u5BB9\u6613\u306B\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002\u306A\u305C\u306A\u3089\u3001TypeScript\u306E\u9759\
  \u7684\u578B\u4ED8\u3051\u306FJavaScript\u30C6\u30B9\u30C8\u306B\u4E88\u6E2C\u53EF\
  \u80FD\u6027\u306E\u30EC\u30D9\u30EB\u3092\u8FFD\u52A0\u3059\u308B\u304B\u3089\u3067\
  \u3059\u3002."
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
