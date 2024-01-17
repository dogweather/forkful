---
title:                "日付を比較する"
html_title:           "Gleam: 日付を比較する"
simple_title:         "日付を比較する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何？なぜ？
日付を比較することは、一つの日付が他の日付よりも前後しているかどうかを確認することです。プログラマーがこれを行う理由は、例えばリマインダーやスケジュールの管理などに役立つからです。

## 方法：
以下は、Gleamで日付を比較する方法の例です。

```Gleam
// 日付を比較する
let equals = Date.equal(date_1, date_2)

// 日付の順序を判定する
let comparison = Date.compare(date_1, date_2)
```

出力は「true」か「false」です。

## 深く掘り下げる：
日付を比較する手段としては、他にも様々な方法があります。例えば、Javaでは```Date.equals()```や```Date.compareTo()```といったメソッドがあります。Gleamでは、日付を保持するのに```Date```型を使用し、それらのメソッドと同じような機能を提供しています。また、内部的には日付はエポック時からのミリ秒単位で表されていることに注意しましょう。

## 関連情報：
日付を比較する方法が気になったら、以下のリンクを参考にしてみてください。

- [Gleam日本語公式サイト](https://gleam.run/ja/)
- [Java Dateクラスのドキュメント](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html)
- [エポック時とは？](https://ja.wikipedia.org/wiki/%E3%82%A8%E3%83%9D%E3%83%83%E3%82%AF%E6%99%82)