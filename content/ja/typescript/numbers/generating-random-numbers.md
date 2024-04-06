---
date: 2024-01-27 20:35:51.649091-07:00
description: "\u65B9\u6CD5 TypeScript\u3067\u306F\u3001\u30B0\u30ED\u30FC\u30D0\u30EB\
  \u306A`Math`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u4F7F\u7528\u3057\u3066\u30E9\
  \u30F3\u30C0\u30E0\u306A\u6570\u5B57\u3092\u751F\u6210\u3067\u304D\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306B\u3001\u7570\u306A\u308B\u8981\u4EF6\u306B\u5BFE\u3057\u3066\u30E9\
  \u30F3\u30C0\u30E0\u306A\u6570\u5B57\u3092\u751F\u6210\u3059\u308B\u305F\u3081\u306E\
  \u5B9F\u7528\u7684\u306A\u4F8B\u3092\u3044\u304F\u3064\u304B\u793A\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-04-05T22:37:50.050642-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5 TypeScript\u3067\u306F\u3001\u30B0\u30ED\u30FC\u30D0\u30EB\u306A\
  `Math`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u4F7F\u7528\u3057\u3066\u30E9\u30F3\
  \u30C0\u30E0\u306A\u6570\u5B57\u3092\u751F\u6210\u3067\u304D\u307E\u3059\u3002\u4EE5\
  \u4E0B\u306B\u3001\u7570\u306A\u308B\u8981\u4EF6\u306B\u5BFE\u3057\u3066\u30E9\u30F3\
  \u30C0\u30E0\u306A\u6570\u5B57\u3092\u751F\u6210\u3059\u308B\u305F\u3081\u306E\u5B9F\
  \u7528\u7684\u306A\u4F8B\u3092\u3044\u304F\u3064\u304B\u793A\u3057\u307E\u3059\u3002"
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

## 方法
TypeScriptでは、グローバルな`Math`オブジェクトを使用してランダムな数字を生成できます。以下に、異なる要件に対してランダムな数字を生成するための実用的な例をいくつか示します。

### 基本的なランダムな数字の生成
0（含む）から1（含まない）の間の基本的なランダムな小数点以下の数を生成するには、`Math.random()`を使用します。これには追加の操作は必要ありません：

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

これは例えば`0.8995452185604771`のような値を出力するかもしれません。

### 特定の二つの値の間のランダムな整数を生成する
特定の二つの値の間の整数が必要な場合、`Math.random()`といくつかの算術を組み合わせて使います：

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

これは例えば1から10の間の整数値、たとえば`7`を出力するかもしれません。

### 一意な識別子の生成
ランダムな数字は他の方法と組み合わせて一意な識別子を作成するために使われます。たとえば、シンプルなUUIDジェネレータのスニペットは次の通りです：

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

これは、例えば`110e8400-e29b-41d4-a716-446655440000`のようなUUIDに似た文字列を生成します。

## 深堀り
JavaScriptおよびそのためにTypeScriptでランダムな数字を生成する主な方法である`Math.random()`は、疑似ランダム数生成器（PRNG）に基づいています。結果はランダムに見えるかもしれませんが、初期シード値に基づいた決定論的なアルゴリズムによって生成されるため、`Math.random()`によって生成される数値は本当にランダムではなく、暗号学的な目的には使用すべきではないことに注意することが重要です。

暗号学的に安全なランダム数のためには、Web Crypto APIは`crypto.getRandomValues()`を提供しており、これはWeb Crypto標準をサポートしている環境（最新のブラウザーやNode.js（`crypto`モジュール経由）など）で利用可能です。以下は、範囲内で安全なランダム数をTypeScriptで生成するための使用例です：

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

この方法はより強度の高いランダム性を提供し、セキュリティに敏感なアプリケーションにより適しています。しかし、これはより多くのリソースを消費するもので、単純なシミュレーションや重要でないランダム値の生成など、さほど重要ではないタスクには必要ないかもしれません。
