---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:42.486154-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Elm\u306B\u306F\u3001\u305D\u306E\
  \u30B3\u30A2\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u30D3\u30EB\u30C8\u30A4\u30F3\u306E\
  regex\u95A2\u6570\u304C\u3042\u308A\u307E\u305B\u3093\u3002\u3053\u308C\u3089\u306E\
  \u64CD\u4F5C\u306B\u306F\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\
  \u3059\u3002regex\u3092\u4F7F\u7528\u3059\u308B\u305F\u3081\u306E\u4EBA\u6C17\u306E\
  \u9078\u629E\u80A2\u306E\u4E00\u3064\u304C `elm/regex` \u3067\u3059\u3002`elm install\u2026"
lastmod: '2024-04-05T22:38:41.538368-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\uFF1A Elm\u306B\u306F\u3001\u305D\u306E\u30B3\
  \u30A2\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u30D3\u30EB\u30C8\u30A4\u30F3\u306Eregex\u95A2\
  \u6570\u304C\u3042\u308A\u307E\u305B\u3093\u3002\u3053\u308C\u3089\u306E\u64CD\u4F5C\
  \u306B\u306F\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u7528\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002\
  regex\u3092\u4F7F\u7528\u3059\u308B\u305F\u3081\u306E\u4EBA\u6C17\u306E\u9078\u629E\
  \u80A2\u306E\u4E00\u3064\u304C `elm/regex` \u3067\u3059\u3002`elm install elm/regex`\u3092\
  \u4F7F\u7528\u3057\u3066\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306B\u8FFD\u52A0\u3067\
  \u304D\u307E\u3059\u3002 \u3053\u308C\u304C\u3001\u3044\u304F\u3064\u304B\u306E\u4E00\
  \u822C\u7684\u306A\u30BF\u30B9\u30AF\u3067`elm/regex`\u3092\u4F7F\u7528\u3059\u308B\
  \u65B9\u6CD5\u3067\u3059\uFF1A."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## どのように：
Elmには、そのコアライブラリにビルトインのregex関数がありません。これらの操作にはサードパーティのライブラリを使用する必要があります。regexを使用するための人気の選択肢の一つが `elm/regex` です。`elm install elm/regex`を使用してプロジェクトに追加できます。

これが、いくつかの一般的なタスクで`elm/regex`を使用する方法です：

### 1. パターンのマッチング
文字列がパターンと一致するかをチェックするには、`Regex.contains`を使用します。

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- 使用例：
isAlphanumeric "Elm2023"     -- 出力： True
isAlphanumeric "Elm 2023!"   -- 出力： False
```

### 2. すべてのマッチを見つける
文字列内のパターンのすべての出現を見つけるには、`Regex.find`を使用します。

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- 使用例：
getWords "Elm is fun!"  -- 出力： ["Elm", "is", "fun"]
```

### 3. テキストの置換
文字列の一部分をパターンに一致するものと置換するには、`Regex.replace`を使用します。

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- 使用例：
replaceElmWithHaskell "Learning Elm is fun!"  
-- 出力： "Learning Haskell is fun!"
```

これらの例では、`Regex.fromString`を使用してregexパターンをコンパイルしています。ここで`\b`は単語の境界にマッチし、`\w`は任意の単語の文字にマッチします。常に`Regex.fromString`の`Maybe`の結果を処理して、無効なregexパターンに対しての安全を確保するべきであり、通常は`Maybe.withDefault`を使用します。
