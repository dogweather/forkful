---
title:                "TypeScript: 「未来と過去の日付の計算」"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
なぜ将来や過去の日付を計算することに関わるのか、わずか1-2文で説明します。

時々、日付を計算する必要があります。例えば、ある特定の日数を加算して特定の日付を求めたり、ある年月日を基準として何日後や何日前の日付を求めたりする場合があります。そんな時、TypeScriptを使えば簡単に日付の計算ができます。

## 使い方
まずは、日付を計算するために必要な日付オブジェクトを作成します。例えば、現在の日付を取得するには以下のように`Date`クラスを使います。

```TypeScript
const today = new Date();
```

次に、将来や過去の日付を計算するために必要な値を定義します。例えば、3日後の日付を求める場合は以下のように`setDate()`メソッドを使います。

```TypeScript
const futureDate = new Date();

futureDate.setDate(today.getDate() + 3);
```

同様に、3日前の日付を求める場合は以下のように`setDate()`メソッドを使います。

```TypeScript
const pastDate = new Date();

pastDate.setDate(today.getDate() - 3);
```

これで、`futureDate`と`pastDate`にそれぞれ3日後と3日前の日付が計算されます。

## 詳細な解説
日付を計算する際には、JavaScriptやTypeScriptに標準で用意されている`Date`クラスを使います。このクラスには、日付や時刻を扱うための様々なメソッドやプロパティが用意されています。

例えば、`setDate()`メソッドは日付を設定するためのものであり、現在の日付に任意の数を加算または減算することができます。また、`getDate()`メソッドを使えば現在の日付を取得することもできます。

他にも、`setFullYear()`や`setMonth()`などのメソッドを使えば、年や月を設定することができます。日付計算に必要なメソッドやプロパティを覚えておくと便利です。

## 相関項目
この記事では 日付の計算について説明しましたが、TypeScriptには日付以外にも様々な便利な機能があります。是非以下のリンクを参考にしてみてください。

[TypeScript 公式ドキュメント](https://www.typescriptlang.org/docs/home.html)
[TypeScript Deep Dive (日本語訳) ](https://typescript-jp.gitbook.io/deep-dive/)

## 参考リンク
[JavaScriptの日付操作まとめ](https://qiita.com/rio_h/items/4039c13bd8f30e7d3081)