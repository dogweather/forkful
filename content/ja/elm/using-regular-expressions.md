---
title:                "正規表現の使用"
aliases:
- ja/elm/using-regular-expressions.md
date:                  2024-02-03T19:16:42.486154-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
