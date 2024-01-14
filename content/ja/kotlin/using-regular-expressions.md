---
title:                "Kotlin: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜRegular Expressionsを使うのか
正規表現はコンピューター上でパターンマッチングを行うための強力なツールです。文字列パターンを比較や検索する際に、時間や手間を節約することができます。

## 使い方
下記のように、Kotlinのコードブロックで実際のプログラミング例と出力を示します。

```Kotlin
// 入力文字列
val input = "こんにちは、私はコーディです。"

// "コーディ"というパターンを正規表現で検索し、見つかった場合は出力する
val pattern = Regex("コーディ")
val result = pattern.find(input)
println("パターンにマッチする文字列は ${result?.value} です。")

// "私[はが]"というパターンを正規表現で置換し、出力する
val replaced = input.replace(Regex("私[はが]"), "僕")
println("置換後の文字列は $replaced です。")
```

出力は次のようになります。

```
パターンにマッチする文字列は コーディ です。
置換後の文字列は こんにちは、僕はコーディです。 です。
```

## 深堀り
正規表現は様々な文字列操作で便利に使われます。例えば、文字列内の特定の単語の出現をカウントすることや、特定の形式に整形することができます。また、文字列のバリデーションやURLの抽出などにも用いることができます。正規表現を熟知することで、より効率的なプログラミングが可能になります。

## さらに見る
- [Kotlin公式ドキュメント: 正規表現](https://kotlinlang.org/docs/regular-expressions.html)
- [入門正規表現](https://codezine.jp/article/detail/11191)
- [正規表現のデバッグ方法](https://uhyohyo.net/software/regex/regex_debugging.html)