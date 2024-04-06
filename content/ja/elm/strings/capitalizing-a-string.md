---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:03.623831-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Elm\u306B\u306F\u3001\
  \u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u305F\u3081\u306E\u5C02\
  \u7528\u306E\u7D44\u307F\u8FBC\u307F\u95A2\u6570\u306F\u3042\u308A\u307E\u305B\u3093\
  \u3002\u3057\u304B\u3057\u3001`toUpper`\u3001`toLower`\u3001`left`\u3001`dropLeft`\u306E\
  \u3088\u3046\u306A\u7D44\u307F\u8FBC\u307F\u306E`String`\u30E2\u30B8\u30E5\u30FC\
  \u30EB\u95A2\u6570\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3001\u3053\u308C\
  \u3092\u7C21\u5358\u306B\u5B9F\u73FE\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-04-05T22:38:41.528930-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Elm\u306B\u306F\u3001\u6587\
  \u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u305F\u3081\u306E\u5C02\u7528\
  \u306E\u7D44\u307F\u8FBC\u307F\u95A2\u6570\u306F\u3042\u308A\u307E\u305B\u3093\u3002\
  \u3057\u304B\u3057\u3001`toUpper`\u3001`toLower`\u3001`left`\u3001`dropLeft`\u306E\
  \u3088\u3046\u306A\u7D44\u307F\u8FBC\u307F\u306E`String`\u30E2\u30B8\u30E5\u30FC\
  \u30EB\u95A2\u6570\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3001\u3053\u308C\
  \u3092\u7C21\u5358\u306B\u5B9F\u73FE\u3067\u304D\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## どのようにして：
Elmには、文字列を大文字化するための専用の組み込み関数はありません。しかし、`toUpper`、`toLower`、`left`、`dropLeft`のような組み込みの`String`モジュール関数を使用することで、これを簡単に実現できます。

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- 例の使用法
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- 出力: "Hello World"
```

より複雑なシナリオや文字列を直接大文字化する機能を提供するライブラリを使用したい場合は、`elm-community/string-extra`のようなサードパーティのパッケージを検討するかもしれません。しかし、最後の更新時点で、Elmのエコシステムは、言語とプロジェクトをすっきりさせるために、そのようなタスクを組み込み関数を使って処理することを推奨しています。

```elm
import String.Extra as StringExtra

-- サードパーティのライブラリに`capitalize`関数がある場合
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- 仮想のライブラリ関数を使った例の使用法
main =
    "this is elm" |> capitalizeWithLibrary
    -- 仮想の出力: "This is elm"
```

文字列操作のための標準ライブラリを超えた追加機能を探している場合は、常にElmパッケージリポジトリをチェックして、文字列操作用の最新で最も好ましいライブラリを確認してください。
