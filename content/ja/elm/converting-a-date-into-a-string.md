---
title:                "日付を文字列に変換する"
html_title:           "Elm: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する必要性は多々あります。例えば、ユーザーが入力した日付を表示する場合やデータベースに格納する前に文字列に変換する必要がある場合などが考えられます。今回は、Elmを使用して日付を文字列に変換する方法について紹介します。

## 方法

まずはDateパッケージをインポートしましょう。

```elm
import Date
```

次に、Dateパッケージの「fromTime」関数を使用して日付を作成します。ここでは、今日の日付を使用しましょう。

```elm
let currentDate = Date.fromTime 0
```

次に、DateTime.Languageパッケージをインポートし、言語を定義します。ここでは、デフォルトの「ja」を使用します。

```elm
import DateTime.Language
let language = DateTime.Language.fromLanguageCode "ja"
```

最後に、「format」関数を使用して日付を文字列に変換します。

```elm
let stringDate = Date.format language "%Y/%m/%d" currentDate
```

上記の例では、日付が「2021年3月23日」となります。

## ディープダイブ

Dateパッケージには多くのフォーマットオプションがあり、日付をより細かく指定することができます。例えば、月や曜日の言語を変更することも可能です。また、「parse」関数を使用することで文字列を日付に変換することもできます。

## はじめてのElm

今回紹介したように、Elmを使用することで日付を簡単に文字列に変換することができます。ぜひ、実際にコードを書いてみて、様々なフォーマットを試してみてください。

## 参考リンク

- [Dateパッケージ - Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [DateTime.Languageパッケージ - Elm Documentation](https://package.elm-lang.org/packages/elm-community/date-time/latest/DateTime-Language)