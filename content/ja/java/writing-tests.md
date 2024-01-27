---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストの書き方とその理由)
テストコードはプログラムが正しく動作することを保証するためのコードです。バグを早期に発見し、機能の追加やリファクタリング時の安心を得るために開発者が行います。

## How to: (やり方)
JUnitを使った基本的なテストの書き方を示します。

```java
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class CalculatorTest {

    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        Assertions.assertEquals(5, calculator.add(2, 3));
    }
}

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
```
実行結果: テストがパスすれば出力はありません。失敗すればエラーが表示されます。

## Deep Dive (深掘り)
JUnitは、1997年にKent Beckによって作られたSUnitをベースにしたJavaのテストフレームワークです。他の代替品としてはTestNGがありますが、JUnitはより普及しています。内部ではリフレクションを使用してテストメソッドを動的に検出し実行します。

## See Also (関連情報)
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Oracle's Java Tutorials – Unit Testing](https://docs.oracle.com/javase/tutorial/java/javaOO/summaryclasses.html)
