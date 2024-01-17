---
title:                "テストの書き方"
html_title:           "Kotlin: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## 何かとなんでやるの？
テストコードを書くことは、コードが正しく機能するかどうかを確認するプログラマーの作業です。プログラマーがテストコードを書くのは、自分の書いたコードに自信を持つためです。

## どうやるの？
```Kotlin
// 例: テストする関数
fun add(a: Int, b: Int): Int {
    return a + b
}

// テストコードの書き方
val result = add(3, 5)
val expected = 8

// テストコードの実行
assert(result == expected) // テストが通れば成功！
```

## 詳しく調べてみると
テストコードを書くことの歴史は古く、今でも主流の開発手法です。他の方法としては、手動でコードをテストしたり、別の人にテストをしてもらったりすることがあります。テストコードを実行する方法には、ユニットテストや結合テストなどがあります。Kotlinでは、JUnitやMockitoといったライブラリがテストコードの書き方をサポートしています。

## こちらも参考に
- [Kotlinの公式ドキュメント](https://kotlinlang.org/docs/home.html)
- [JUnit公式サイト](https://junit.org/junit5/)
- [Mockito公式サイト](https://site.mockito.org/)