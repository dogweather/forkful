---
title:                "正規表現を利用する"
html_title:           "Elm: 正規表現を利用する"
simple_title:         "正規表現を利用する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ正規表現を使うのか

正規表現を使うと、文字列のパターンを簡単にマッチングしたり置換したりすることができます。例えば、特定の文字列を含むすべての文章を検索したり、メールアドレスの形式が正しいかどうかを確認したりするのに便利です。

## 使い方

```elm
-- 文字列のマッチング
Regex.contains (Regex.regex "Hello") "Hello, world!" -- True

-- パターンを使った文字列の置換
Regex.replace (Regex.regex "[a-zA-Z]") (\_ -> "x") "Hello, world!" -- "xxxxx, xxxxx!"

-- パターンを使ったグループ化と抽出
Regex.find (Regex.regex "([0-9]+)月([0-9]+)日") "今日は12月25日です。" -- Just ["12月25日","12","25"]
```

## ディープダイブ

正規表現は強力なツールですが、パターンを正しく記述することが重要です。間違ったパターンを使うと、意図しない結果になることがあります。また、正規表現を使うときには、パフォーマンスのことを考える必要もあります。

## 関連情報

* [Elmの正規表現ドキュメント](https://package.elm-lang.org/packages/elm/regex/latest/)
* [正規表現の基礎 (W3C)](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
* [正規表現の威力 (Qiita)](https://qiita.com/jesus_isao/items/e2dc1b0842148b3f00a7)