---
title:                "「文字列から日付を解析する」"
html_title:           "Haskell: 「文字列から日付を解析する」"
simple_title:         "「文字列から日付を解析する」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

何をするため？
日付を文字列から解析するとは、プログラマーが文字列から日付の情報を抽出することです。このようなことをするのは、プログラムがユーザーから入力された日付情報を正しく処理できるようにするためです。

方法：

```Haskell
parseDate :: String -> Maybe Day
parseDate str = parseTimeM True defaultTimeLocale "%m/%d/%Y" str :: Maybe Day
```

サンプル出力：

```
parseDate "05/12/2020" -- Just 2020-05-12
parseDate "13/05/2020" -- Nothing (invalid date)
```

深く掘る：

日付を解析する方法は、プログラミング言語によって異なります。Haskellでは、parseTimeM関数を使用して、指定されたフォーマットに従って文字列から日付を解析します。他の言語では、正規表現や組み込みの日付パーサーを使用することもできます。

また、日付を文字列から解析する方法は、既存のライブラリを使用することもできます。例えば、HaskellのDateStringライブラリは、さまざまな書式の日付を解析することができます。

参考リンク：

- `parseTimeM` 関数のドキュメント: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html#v:parseTimeM
- DateString ライブラリのドキュメント: https://hackage.haskell.org/package/datestring