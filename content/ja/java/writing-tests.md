---
title:    "Java: 「テストの書き方」"
keywords: ["Java"]
---

{{< edit_this_page >}}

## なぜテストを書くのか

プログラミングには、エラーを見つけて修正するためのテストという非常に重要なプロセスがあります。テストを書くことで、コードを綺麗に保ち、より効率的な開発を行うことができます。

## テストの書き方

テストを書く方法を理解するために、Javaコードを使用したシンプルな例を見てみましょう。

```Java
import org.junit.Test;
import static org.junit.Assert.*;

public class CalculatorTest {

    @Test
    public void testAdd() {
        int result = Calculator.add(2, 3);
        assertEquals(5, result);
    }

    @Test
    public void testMultiply() {
        int result = Calculator.multiply(2, 3);
        assertEquals(6, result);
    }
}
```

このコードでは、JUnitの`assertEqual()`メソッドを使用して、期待される結果と実際の結果を比較しています。これにより、テストが成功したかどうかを確認することができます。

## テストの詳細について

テストを書く際にはいくつかのポイントに気をつける必要があります。最初に、テストが予期しない結果を返すことを確認する必要があります。また、テストを書く際には、各テストが正しいデータを使用して実行されることを確認する必要があります。さらに、テストが最低限のコードで書かれていることを確認し、無駄な重複を避ける必要があります。

## See Also

- [JUnit 公式ドキュメント](https://junit.org/junit5/docs/current/user-guide/)
- [TDD (Test Driven Development) について](https://www.ibm.com/developerworks/jp/java/library/j-tdd/index.html)
- [代表的な Java テストフレームワークまとめ](https://qiita.com/tag1216/items/e009e982cef6b9a711ce)