---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:05.577769-07:00
description: "\u65B9\u6CD5: Google Apps Script \u3067\u306F\u3001JavaScript \u3068\
  \u540C\u69D8\u306B\u3001`Math.random()`\u2026"
lastmod: '2024-03-13T22:44:41.438735-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u3067\u306F\u3001JavaScript \u3068\u540C\u69D8\u306B\
  \u3001`Math.random()` \u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u4E71\u6570\u3092\
  \u751F\u6210\u3067\u304D\u307E\u3059\u3002\u3053\u306E\u95A2\u6570\u306F\u30010\uFF08\
  \u542B\u3080\uFF09\u304B\u30891\uFF08\u542B\u307E\u306A\u3044\uFF09\u306E\u7BC4\u56F2\
  \u306E\u6D6E\u52D5\u5C0F\u6570\u70B9\u306E\u64EC\u4F3C\u4E71\u6570\u3092\u8FD4\u3057\
  \u307E\u3059\u3002\u7279\u5B9A\u306E\u7BC4\u56F2\u5185\u3067\u6574\u6570\u3092\u751F\
  \u6210\u3059\u308B\u306A\u3069\u3001\u3055\u307E\u3056\u307E\u306A\u30E6\u30FC\u30B9\
  \u30B1\u30FC\u30B9\u306E\u305F\u3081\u306B\u3053\u308C\u3089\u306E\u6570\u5024\u3092\
  \u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3059\u308B\u306B\u306F\u3001\u8FFD\u52A0\u306E\
  \u8A08\u7B97\u3092\u884C\u3046\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059."
title: "\u30E9\u30F3\u30C0\u30E0\u6570\u306E\u751F\u6210"
weight: 12
---

## 方法:
Google Apps Script では、JavaScript と同様に、`Math.random()` 関数を使用して乱数を生成できます。この関数は、0（含む）から1（含まない）の範囲の浮動小数点の擬似乱数を返します。特定の範囲内で整数を生成するなど、さまざまなユースケースのためにこれらの数値をカスタマイズするには、追加の計算を行う必要があります。

### 基本的な乱数を生成する
単純な乱数を生成してコンソールに記録するには：

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*サンプル出力:* `0.1234567890123456`

### 特定の範囲内の整数を生成する
2つの値（`min` と `max`）の間の乱数整数を生成するには、値を丸括弧で囲みます：

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// 例:
getRandomInt(1, 10);
```
*サンプル出力:* `7`

最小値を切り上げるために `Math.ceil()` 関数が使用され、最大値を切り捨てるために `Math.floor()` が使用されていることに注意してください。これにより、乱数が指定した範囲内になることが保証されます。

## 詳細な解説
Google Apps Script やほとんどのプログラミング言語で乱数を生成するメカニズムは、疑似乱数生成器（PRNG）を利用しています。この技術は決定論的であり、シードとして知られる初期値に依存して、ランダムに見える数値のシーケンスを生成します。多くのアプリケーションにはこれで十分ですが、高いセキュリティや真のランダム性が求められる場合（例えば暗号化アプリケーションなど）には、疑似乱数が適切ではない可能性があることに注意が必要です。

真のランダム性は、ハードウェア乱数生成器や自然現象からランダム性を生成するサービスを通じて実現できます。しかし、Google Apps Script でのほとんどの日常的なスクリプトニーズには、`Math.random()` が十分です。

歴史的には、より効果的な乱数生成技術を求めるクエストがさまざまなアルゴリズムの開発につながりました。その注目すべき例としてメルセンヌ・ツイスターと線形合同法（LCG）があります。ただし、Google Apps Script の高レベルの抽象化を考えると、ほとんどのユーザーがこれらのアルゴリズムを直接実装する必要はありませんが、スクリプト内で乱数生成の重要性と限界を理解するのに役立つことでしょう。
