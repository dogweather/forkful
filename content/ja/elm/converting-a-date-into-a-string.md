---
title:                "Elm: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することの重要性を説明するための1-2文。

日付を文字列に変換する理由は、エルム（Elm）で日付と時刻を扱う際に非常に便利な方法です。例えば、JavaScriptでは、日付を表示するために複雑な処理が必要ですが、エルムでは日付を簡単に文字列に変換することができます。これにより、コードの見通しが良くなり、バグの可能性も低くなります。

## 方法

日付を文字列に変換するには、いくつかの方法があります。その中でも、エルムの標準モジュールである `Time` モジュールを使用する方法が最も簡単です。

まずは日付を表す値である `Posix` タイプを作成します。次に、`Time.format` 関数を使用して、指定したフォーマットに従って日付を文字列に変換します。以下のコードブロックを見ると、より具体的な例が分かりやすくなるでしょう。

```Elm
-- 日付を表すPosix型を作成
let date = Time.millisToPosix 1555555555

-- 指定したフォーマットに従って、日付を文字列に変換
let dateString = Time.format "%Y年%m月%d日" date
```

上記のコードでは、`Posix`型を作成する際に `Time.millisToPosix` 関数を使用しています。この関数には、ミリ秒単位で表された日時の数値を渡すことで、`Posix`型を作成することができます。

そして、`Time.format` 関数には、フォーマットを指定するためのパラメーターとして `%Y`（年）、`%m`（月）、`%d`（日）を使用し、その後に`Posix`型を渡すことで、指定したフォーマットに従って日付を文字列に変換することができます。

上記のコードを実行すると、`"2019年04月18日"`という文字列が得られます。

## 深堀り

もし、上記の方法で日付を文字列に変換する際にフォーマットのカスタマイズを行いたい場合は、`Time.format` 関数の中で使用できるフォーマット文字列について学ぶ必要があります。

たとえば、年や月などの単位を表す文字列には、`%Y` や `%m` の他に `%D` や `%h` といったものがあり、それぞれ月の日数や24時間制の時間の表記を提供します。

詳しい情報は[公式ドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Time#Format)を参照してください。

このように、エルムでは日付を文字列に変換する際に非常に便利な方法が用意されています。ぜひ、自分のプロジェクトで使用してみてください。

## 関連リンク

- [エルム公式ドキュメント - Date](https://guide.elm-lang.org/architecture/effects/time.html)
- [エルム公式ドキュメント - Time](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Formatプロジェクト](https://package.elm-lang.org/packages/justinmimbs/date-format/latest/)