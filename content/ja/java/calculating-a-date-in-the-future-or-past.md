---
title:    "Java: 未来または過去の日付を計算する"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

将来の日付や過去の日付を計算することに関心を持つ理由は、日常生活でよくあることです。例えば、あなたの誕生日や結婚記念日を計画するために、いつそれが何曜日に当たるのかを知りたいかもしれません。

## 方法

今回は、Javaを使用して将来の日付や過去の日付を計算する方法を説明します。以下のコードブロックには、具体的なコーディング例とその出力が含まれています。

```Java
// 今日の日付を取得
LocalDate today = LocalDate.now();

// 3ヶ月後の日付を計算
LocalDate futureDate = today.plusMonths(3);

// 計算結果を出力
System.out.println("3ヶ月後の日付は: " + futureDate);
```

このコードを実行すると、現在の日付から3ヶ月後の日付が計算され、次のような出力が得られるでしょう。

```text
3ヶ月後の日付は: 2021-07-20
```

同様に、過去の日付も計算することができます。例えば、30年前の日付を計算する場合は、`today.minusYears(30)`というようにコードを変更すれば良いでしょう。

## 深堀り

Javaでは、`LocalDate`クラスを使用して日付を表現します。上記の例では、`now()`メソッドを使って今日の日付を取得し、`plusMonths()`メソッドを使って指定した月数を加算しています。

また、`LocalDate`クラスには、日付の比較やフォーマットなどを行うための便利なメソッドが多数用意されています。必要に応じて、これらのメソッドを使用して日付計算をより複雑なものにすることができます。

## もっと詳しく知る

Javaを使用して日付を計算する方法については、公式ドキュメントなどさまざまな情報源があります。以下のリンクを参考に、さらに詳しく学習してみてください。

- [Java LocalDateクラスドキュメント (英語)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [日付と時刻を扱うためのJava標準ライブラリの使い方 (日本語)](https://future-architect.github.io/articles/20190831/)
- [Java 8での日付・時刻まとめ (日本語)](https://qiita.com/uchiko/items/f141bf9926029c4f0553)

## 参考リンク

- [Javaで現在の日付と時刻を取得する方法 (日本語)](https://www.javadrive.jp/start/date/)
- [JavaのDateTime APIの使い方 (日本語)](https://insideyou.tokyo/how-to-use-java-datetime-api/)