---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ？何のために？

パターンに一致する文字を削除するとは、特定の規則またはパターンに一致する文字や文字列を削除することを指します。これをプログラマはコードから不要な部分を取り除いたり、データをクリーンアップしたりするために行います。

## 実装方法：

以下のコードは、文字列から特定のパターンを探して削除します。具体的には、「abc123xyz」から数字を削除します。

```Kotlin
fun main() {
    var str = "abc123xyz"
    val regex = "[0-9]".toRegex()
    str = regex.replace(str, "")
    println(str) /* "abcxyz" を出力 */
}
```
このコードは、正規表現 "[0-9]"を用いて数字を特定し、それらを空の文字列で置き換えることで削除します。

## 深掘り：

歴史的な文脈では、文字やパターンの削除は古くから存在しており、さまざまな文字列操作アルゴリズムの包括的な部分として考えられてきました。またJavaではStringBuilderやStream APIを使用し、またPythonではlist comprehensionやreモジュールを使用するといった方法があります。

パターンマッチングの削除は、效率やパフォーマンスに重要な役割を果たします。Kotlinでは、正規表現クラスを用いてこれを実装します。また、`replace`関数を用いて該当する部分を置換します。効率を最大化するためには、操作対象の文字列が大きい場合や複数回の操作が必要な場合は`StringBuilder`を使うことを考慮すべきです。 

## 参考文献：

正規表現に関する詳細：
https://docs.oracle.com/javase/tutorial/essential/regex/

文字列操作についての詳しい情報：
https://developer.android.com/reference/kotlin/java/lang/StringBuilder

パターンマッチと削除の一部となるアルゴリズムについて：
http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html