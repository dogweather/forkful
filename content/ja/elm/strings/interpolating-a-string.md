---
title:                "文字列の補間"
aliases: - /ja/elm/interpolating-a-string.md
date:                  2024-01-20T17:50:56.093510-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
文字列補間は、文字列の中に変数や計算結果を埋め込むことです。プログラマーはコードの可読性とメンテナンス性を上げるためにこれを行います。

## How to: (実施方法)
Elmには文字列補間が直接的な方法で提供されていないため、`String`モジュールの関数を使って同様のことをします。

```Elm
name = "世界"
greeting = "こんにちは, " ++ name ++ "!"

-- 出力
"こんにちは, 世界!"
```

`++` 演算子を利用して文字列を連結することにより、変数を文字列の中に挿入しています。

## Deep Dive (深掘り)
Elmが文字列補間を直接的な文法でサポートしていないのは、Elmのシンプリシティと厳密なタイプシステムに根ざしています。JavaScriptや他の言語ではバックティック（` ` `）と${}を用いた文字列補間がありますが、Elmでは文字列連結を推奨します。これはElmの全体的な設計方針の一部である、誤りを発生させる余地を減らすような安全な言語機能に整合しています。

他の代替としては、`String.join` やフォーマット文字列を使った方法がありますが、これらも基本的には文字列を連結する操作です。

```Elm
hello name =
  String.join "" ["やあ、 ", name, "! どうもです。"]

-- 使用例
hello "ボブ"

-- 出力
"やあ、 ボブ! どうもです。"
```

Elmでは文字列の内部表現も重要です。Elmの文字列はUTF-8でエンコードされていて、国際化をサポートするための確固たる基盤があります。これは日本語を含む多言語の文字列操作に重要です。

## See Also (関連情報)
- Elmの`String`モジュールのドキュメント: [公式ドキュメント](http://package.elm-lang.org/packages/elm/core/latest/String)
- Elm言語の全体設計について深く知る: [Elm公式ガイド](https://guide.elm-lang.org/)
