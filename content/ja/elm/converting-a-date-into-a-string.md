---
title:                "Elm: 日付を文字列に変換する"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

**日本語読者のための、カジュアルなElmプログラミングのブログ投稿**

## なぜ
日付の変換を文字列にすることが必要か、その理由をご存知ですか？Elmでは、日付を文字列に変換することで、より柔軟なプログラミングが可能になります。たとえば、日付のフォーマットを簡単に変更したり、特定の日付を比較したりすることができます。

## 方法
以下のコード例を参考にして、日付を文字列に変換する方法を紹介します。

```Elm
import Date exposing (Date, month, day, year)
import Time exposing (toString)

myDate : Date
myDate = Date.fromParts 2020 Apr 1  -- 日付を作成

toString (year myDate) ++ "/" ++ toString (month myDate) ++ "/" ++ toString (day myDate)
-- 結果： "2020/4/1"
```

このように、`Date`モジュールの`toString`関数を使用することで、日付を文字列に変換することができます。また、日付のフォーマットも自由に調整できるので、自分のプログラムに合わせてカスタマイズすることが可能です。

## 深堀り
日付を文字列に変換する際、Elmでは様々なオプションが用意されています。たとえば、`Date`モジュールでは、すばらしい関数を提供しています。例えば、既存の日付に対して、30日前や2年後などの計算を行うことができます。また、`Time`モジュールでは、様々なフォーマットオプションが用意されています。こういった便利な機能を活用することで、より使いやすいアプリケーションを開発することができます。

## 関連リンク
"See Also (参考):" 
- [Elm Documentation](https://guide.elm-lang.org/)
- [Elm Japan User Group](https://elmjapan.org/)
- [Elm Package Repository](https://package.elm-lang.org/)