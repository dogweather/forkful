---
title:                "ランダム数の生成"
aliases:
- ja/google-apps-script/generating-random-numbers.md
date:                  2024-02-01T21:54:05.577769-07:00
model:                 gpt-4-0125-preview
simple_title:         "ランダム数の生成"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
