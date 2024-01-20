---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列から部分文字列を抽出するとは、大きな文字列の具体的な一部を取り出すことです。プログラマーはこれを行う理由は多岐にわたりますが、主には特定のデータを分析したり、加工したりするためです。

## どのように行うか:

Kotlinでは、`substring()`関数を使って部分文字列を抽出できます。サンプルコードを見てみましょう。

```Kotlin
val 字句 = "Kotlinプログラミング"
val 部分字句 = 字句.substring(0,6)
println(部分字句)
```
出力:

```
Kotlin
```

上記の例では、"Kotlinプログラミング"から"Kotlin"を抽出しています。

## より深く

- **歴史**: 部分文字列の抽出は、文字列処理における基本的な操作です。これが可能になったのは、プログラミング言語が文字列を操作する能力を開発するために、ASCIIやUnicodeといった文字エンコーディングが進化してきたからです。
- **代替案**: Kotlinでは、他にも`substringAfter()`, `substringBefore()`, `substringAfterLast()`, `substringBeforeLast()`なといった方法も提供しています。使い方はシチュエーションによります。
- **実装詳細**: Kotlinの`substring()`関数は、コンピュータ上のメモリ内で文字列を扱うための効率的な方法を使用します。部分文字列は新しい文字列を作成せず、元の文字列の参照を保持します。

## 参照資料

- [Kotlin Official Documentation on Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)