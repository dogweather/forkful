---
title:    "Java: テストの書き方"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか？

ソフトウェア開発では、バグを未然に防止したり、変更したコードが正しく動作することを確認するためにテストが欠かせません。テストを書くことで、開発者は自分のコードに対する自信を持つことができます。

## テストの書き方

以下のコードブロックには、Javaプログラミングでテストを書く方法を示すサンプルコードと出力が含まれています。

```Java
// テストクラスの作成
public class CalculatorTest {

    // テストメソッド
    @Test // テストメソッドであることを示すアノテーション
    public void testAddition() {

        Calculator calculator = new Calculator();

        // テストしたい結果と比較
        assertEquals(10, calculator.add(5,5));
    }
}
```

上記のように、テストクラスを作成し、`@Test`アノテーションを使用してテストメソッドを作成します。そして、テストしたい結果と比較するアサーションを用いてテストを実行します。

## テストについて深く掘り下げる

テストでは、プログラムの各部分が意図した通りに機能するかどうかを確認するために、様々な種類のテストを使用します。例えば、ユニットテスト、統合テスト、受け入れテストなどがあります。また、テストカバレッジの概念も重要で、コードのどの部分がテストされているかを測定します。

## See Also

- [JUnit test framework](https://junit.org/)
- [Test-driven development (TDD)](https://en.wikipedia.org/wiki/Test-driven_development)
- [Code coverage in software testing](https://en.wikipedia.org/wiki/Code_coverage)