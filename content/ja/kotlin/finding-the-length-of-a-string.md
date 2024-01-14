---
title:    "Kotlin: 文字列の長さを見つける"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の長さを求めることに興味があるのは、プログラムを書く上でとても重要なタスクだからです。文字列の長さを知ることで、文字列内のデータをより効率的に処理することができます。

## 方法
文字列の長さを求める方法は非常に簡単です。まず最初に、文字列を変数に格納します。その次に、`length`関数を使って変数の文字列の長さを求めます。以下のKotlinコードを参考にしてみてください。

```Kotlin
var myString = "こんにちは！"
var length = myString.length
println("文字列の長さは $length です。")
```

出力結果は以下のようになります。

```
文字列の長さは 5 です。
```

## 深堀り
文字列の長さを求めるために使用される`length`関数は、内部的には`String`クラスの`length()`メソッドを呼び出します。このメソッドは文字列のUTF-16コード単位の数を返します。つまり、日本語の1文字は2つのUTF-16コード単位で構成されるため、`length`関数は日本語の文字数を正しくカウントすることができます。

また、`length`関数は`CharSequence`インターフェースが実装する拡張メソッドでもあります。そのため、文字列以外のデータ構造でも`length`関数を使用することができます。

## 参考
- [Kotlin - Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Java - String#length](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
- [Kotlin - CharSequence.length](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-char-sequence/length.html)

## 関連リンク
- [文字列の操作について学ぼう！](https://www.sejuku.net/blog/64500)
- [Kotlinの基本的な文法を学ぶ](https://qiita.com/yuto_1014/items/ec1ecb2f3c4c8758e73b)