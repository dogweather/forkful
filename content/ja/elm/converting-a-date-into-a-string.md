---
title:    "Elm: 日付を文字列に変換する"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

こんにちは、みなさん！今日はElmのプログラミングについてお話ししたいと思います。Elmは、関数型言語であり、そのパワフルな型システムと簡潔な構文で人気があります。今回は、日付を文字列に変換する方法について説明します。

## Why
プログラマーは、アプリケーションやウェブサイトで日付を表示する必要があります。しかし、時には日付を文字列に変換する必要がある場合があります。例えば、データベースから取得した日付を、特定のフォーマットで表示したい場合や、ローカルの言語に合わせて日付を表示したい場合です。Elmでは、日付を文字列に変換するための便利な関数が用意されています。

## How To
まずは、文字列に変換したい日付をDate型で定義します。例えば、"2021/12/25"という日付を表示する場合は、以下のように定義します。

```Elm
date = Date.fromString "2021/12/25"
```

次に、DateTimeモジュールのtoString関数を使って、日付を文字列に変換します。例えば、"yyyy/MM/dd"という形式で日付を表示するには、以下のように記述します。

```Elm
DateTime.toString "yyyy/MM/dd" date |> Result.withDefault ""
-- output: "2021/12/25"
```

日付のフォーマットは、"yyyy"や"MM"などのパラメータを組み合わせることで自由に設定できます。詳細なパラメータについては、公式ドキュメントを参照してください。

## Deep Dive
DateTimeモジュールのtoString関数では、内部的にJavaScriptのDateオブジェクトを使って日付を文字列に変換しています。そのため、日付のフォーマットに関するJSの仕様に合わせて変換されます。また、ブラウザによっても多少の違いがあるため、必要に応じてテストを行うことを推奨します。さらに、日付をフォーマットするだけでなく、Time.Zoneモジュールを使ってタイムゾーンを指定することもできます。

## See Also
- Official Elm Documentation on Date and Time: https://package.elm-lang.org/packages/elm/core/latest/Date-Time
- Elm Date Format package: https://package.elm-lang.org/packages/owanturist/elm-date-format/latest/