---
title:                "パターンに一致する文字を削除する"
html_title:           "Kotlin: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

あなたはKotlinで文字列から特定のパターンにマッチする文字を削除する必要があるかもしれません。この記事では、その理由と方法を解説します。

## 方法

```
Kotlin val string = "Hello! This is a sample string." 
val pattern = "\\p{Punct}".toRegex() 
val result = string.replace(pattern, "") 
println(result)
```

このコードを実行すると、以下の結果が得られます。

```
Hello This is a sample string
```

`replace()`関数を使用して、マッチする文字を空文字に置換することで、簡単に文字列から特定のパターンにマッチする文字を削除することができます。

## ディープダイブ

もしもあなたが文字列からマッチする文字を1文字ずつ削除する場合、以下のようなコードを書くことができます。

```
Kotlin fun deleteChar(string: String, pattern: String): String { 
    var result = "" 
    for(char in string) { 
        if(!pattern.contains(char)) { 
            result += char 
        } 
    } 
    return result 
}

val string = "Hello! This is a sample string." 
val pattern = "\\p{Punct}"
val result = deleteChar(string, pattern) 
println(result)
```

しかし、`replace()`関数を使用することで、より簡潔で効率的なコードを書くことができます。また、`replace()`関数には正規表現を使用することができるため、より柔軟に文字を削除することができます。

## 参考リンク

- Kotlin公式ドキュメント: https://kotlinlang.org/docs/reference/strings.html
- 正規表現チュートリアル: https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions