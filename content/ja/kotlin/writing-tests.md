---
title:                "Kotlin: テストの書き方"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

アプリケーションの品質を確保するためには、テストが不可欠です。テストを書くことで、バグを減らし、コードの安定性を保つことができます。

## テストの書き方

テストを書くためには、まずテストフレームワークを導入する必要があります。 Kotlinの場合は、 `AndroidJunitRunner` を使用することで、JUnitテストを実行できます。

```Kotlin
@RunWith(AndroidJUnitRunner::class)
class MyTest {
    @Test
    fun testSomething() {
        // テストコードをここに書く
    }
}
```

テストメソッドの前には `@Test` アノテーションをつけて、テストする内容を書きます。例えば、ある関数が正しい結果を返すかどうかをテストする場合は、`assertEquals()` メソッドを使用します。

```Kotlin
@Test
fun testDouble() {
    val result = someFunction(2)
    assertEquals(4, result)
}
```

`assertEquals()` メソッドの第1引数には、期待する結果を、第2引数には実際の結果を指定します。テストが成功すると、緑のマークが出力され、失敗すると赤のマークが出力されます。

## テストの深堀り

テストを書く際には、どのようなテストを書けばよいか悩むことがあります。しかし、心配しなくて大丈夫です。コードカバレッジツールを使用することで、テストを書くべき箇所を把握することができます。

また、TDD（Test Driven Development）を実践することで、テストを先に書くことでコードの設計を改善することができます。これにより、バグを事前に防ぐことができます。

## 関連リンク

- [JUnit | Android Developers](https://developer.android.com/training/testing/unit-testing/local-unit-tests)
- [Code Coverage Tools for Kotlin | Medium](https://medium.com/@codemonkey86/code-coverage-tools-for-kotlin-13993a35de36)
- [TDDとは？TDDのメリット・デメリットと始め方 | Wantedly Blog](https://www.wantedly.com/companies/32fun/blog_articles/140085)
- [Kotlin + TDDで始めるAndroidアプリ開発 | Zenn](https://zenn.dev/kazuki43zoo/articles/b44b9381fc7f8d1ab03c)