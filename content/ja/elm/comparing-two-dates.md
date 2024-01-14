---
title:    "Elm: 「2つの日付を比較する」"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## なぜ

日付を比較することの利点は何でしょうか？実際には、日付の比較はよく使用されるプログラミングタスクの一つです。例えば、特定のイベントが終了する日付をチェックしたり、期限が過ぎているかどうかを確認したりする際に役立ちます。

## 方法

まず、日付をエポック秒に変換します。それから、Elmの``Time``モジュールにある``compare``関数を使用して日付を比較することができます。下記のコード例を見てみましょう。

```
-- 今日と明日の日付
today = Time.now
tomorrow = Time.tomorrow

-- 日付をエポック秒に変換
todayInSeconds = Time.toPosix today
tomorrowInSeconds = Time.toPosix tomorrow

-- 日付を比較
comparison = Time.compare todayInSeconds tomorrowInSeconds

-- 結果をコンソールに出力
Debug.log "比較結果" comparison
```

このコードを実行すると、``comparison``変数に値が格納されます。``equals``、``lessThan``、``greaterThan``のいずれかを返します。これらの値を使用して、日付の比較結果を判断することができます。

また、``Time``モジュールには``toIsoString``関数もあります。これを使用すると、日付の文字列を比較することもできます。詳しくは、公式ドキュメントを参照してください。

## 深堀り

日付の比較にはいくつかの注意点があります。まず、日付を``Time``モジュールで扱う場合、Timezoneを考慮する必要があります。また、日付を文字列として比較する場合、文字列のフォーマットが同じであることが重要です。例えば、"2021/01/01"と"01/01/2021"では結果が異なります。

## 併せて読みたい

- [Elm製のタイムゾーンライブラリの紹介 (英語)](https://elmprogramming.com/timezone-library-elm.html)
- [日付の処理についてのElmの公式ドキュメント (英語)](https://package.elm-lang.org/packages/elm/time/latest/)
- [日付の比較を行える別の言語 (日本語)](https://qiita.com/satoyan419/items/2865cc170f6992a8f12b)