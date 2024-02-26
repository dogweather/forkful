---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:42.486154-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u6B63\
  \u898F\u8868\u73FE\uFF08regex\uFF09\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\u4E2D\
  \u306E\u6587\u5B57\u306E\u7D44\u307F\u5408\u308F\u305B\u3092\u30DE\u30C3\u30C1\u30F3\
  \u30B0\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u308B\u30D1\u30BF\u30FC\
  \u30F3\u3067\u3059\u3002Elm\u306B\u304A\u3044\u3066\u3082\u3001\u4ED6\u306E\u8A00\
  \u8A9E\u3068\u540C\u69D8\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u5165\u529B\
  \u306E\u691C\u8A3C\u3001\u691C\u7D22\u3001\u6587\u5B57\u5217\u5185\u306E\u30C6\u30AD\
  \u30B9\u30C8\u306E\u7F6E\u63DB\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306Bregex\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002\u305D\u306E\u67D4\u8EDF\u6027\u3068\u52B9\u7387\
  \u306E\u305F\u3081\u3067\u3059\u3002"
lastmod: '2024-02-25T18:49:40.022902-07:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u6B63\
  \u898F\u8868\u73FE\uFF08regex\uFF09\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\u4E2D\
  \u306E\u6587\u5B57\u306E\u7D44\u307F\u5408\u308F\u305B\u3092\u30DE\u30C3\u30C1\u30F3\
  \u30B0\u3059\u308B\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u308B\u30D1\u30BF\u30FC\
  \u30F3\u3067\u3059\u3002Elm\u306B\u304A\u3044\u3066\u3082\u3001\u4ED6\u306E\u8A00\
  \u8A9E\u3068\u540C\u69D8\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u5165\u529B\
  \u306E\u691C\u8A3C\u3001\u691C\u7D22\u3001\u6587\u5B57\u5217\u5185\u306E\u30C6\u30AD\
  \u30B9\u30C8\u306E\u7F6E\u63DB\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306Bregex\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002\u305D\u306E\u67D4\u8EDF\u6027\u3068\u52B9\u7387\
  \u306E\u305F\u3081\u3067\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
プログラミングにおける正規表現（regex）とは、文字列の中の文字の組み合わせをマッチングするために使用されるパターンです。Elmにおいても、他の言語と同様、プログラマーは入力の検証、検索、文字列内のテキストの置換などのタスクにregexを使用します。その柔軟性と効率のためです。

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
