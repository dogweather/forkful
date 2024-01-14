---
title:                "Javascript: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日常生活やビジネスにおいて、将来または過去の日付を計算することは非常に便利です。例えば、会議やイベントの日程を調整する際、ある日から何日後に起こるイベントを調べたい時などに役立ちます。Javascriptを用いることで簡単に日付を計算することができます。

## 使い方

日付を計算する際には、まずはJavascriptで現在の日付を取得する必要があります。以下のコードブロックを参考にしてください。

```Javascript
const now = new Date(); // 現在の日付を取得
```

次に、日付を計算したい日数を与えます。例えば、10日後の日付を計算したい場合は以下のようになります。

```Javascript
const futureDate = new Date(now.getTime() + (10 * 24 * 60 * 60 * 1000)); // 10日後の日付を計算
```

以上の方法で、現在の日付から指定した日数後の日付を計算することができます。また、過去の日付を計算する場合は引き算を行えば同様に計算できます。

## 深堀り

JavascriptではDateオブジェクトを利用することで日付を扱うことができます。DateオブジェクトはUTC時間を基準としており、getTime()メソッドを用いることでミリ秒単位で日付を取得することができます。そのため、指定した日数をミリ秒に変換し加算することで簡単に日付を計算することができます。

また、JavascriptではMoment.jsというライブラリを使用することでより柔軟に日付を扱うことができます。Moment.jsには様々な便利なメソッドが用意されており、より高度な日付計算を行うことができます。

## 参考リンク

- [Javascriptで日付を取得する方法](https://qiita.com/sarah/items/0dea669944bcc9d159d0)
- [Javascriptで日付を計算する方法](https://qiita.com/morrr/items/1854c8dfc59a0f63bcdd)
- [Moment.js公式ドキュメント](https://momentjs.com/docs/)