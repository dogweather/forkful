---
aliases:
- /ja/google-apps-script/generating-random-numbers/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:05.577769-07:00
description: "\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u3053\u3068\u306F\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u57FA\u672C\u7684\u306A\
  \u30BF\u30B9\u30AF\u3067\u3042\u308A\u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\
  \u30F3\u3001\u30B2\u30FC\u30E0\u3001\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u30B7\u30B9\
  \u30C6\u30E0\u306A\u3069\u3001\u591A\u304F\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u3067\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001Google \u30A8\u30B3\u30B7\u30B9\u30C6\u30E0\u5185\u306E\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\uFF08Sheets\u3001Docs\u3001Forms\u2026"
lastmod: 2024-02-18 23:08:54.518286
model: gpt-4-0125-preview
summary: "\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u3053\u3068\u306F\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u57FA\u672C\u7684\u306A\
  \u30BF\u30B9\u30AF\u3067\u3042\u308A\u3001\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\
  \u30F3\u3001\u30B2\u30FC\u30E0\u3001\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u30B7\u30B9\
  \u30C6\u30E0\u306A\u3069\u3001\u591A\u304F\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u3067\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001Google \u30A8\u30B3\u30B7\u30B9\u30C6\u30E0\u5185\u306E\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\uFF08Sheets\u3001Docs\u3001Forms\u2026"
title: "\u30E9\u30F3\u30C0\u30E0\u6570\u306E\u751F\u6210"
---

{{< edit_this_page >}}

## 何となぜ？

乱数を生成することは、プログラミングにおける基本的なタスクであり、シミュレーション、ゲーム、セキュリティシステムなど、多くのアプリケーションで使用されます。プログラマーは、Google エコシステム内のアプリケーション（Sheets、Docs、Forms など）に可変性を導入し、シナリオをテストし、予測不可能性を加えるために、Google Apps Script でこの技術を使用します。

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
