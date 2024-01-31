---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストコードの「何」と「なぜ」)

テストコードとは、プログラムが意図した通りに動いているか確認するためのコードです。これはバグの早期発見、将来のコード変更時の信頼保持、さらに品質を担保するために不可欠です。

## How to: (やり方)

KotlinではJUnitを使ってテストを書きます。基本的なテストの例を以下に示します。

```kotlin
import org.junit.Assert.*
import org.junit.Test

class CalculatorTest {

    @Test
    fun `足し算が正しく行われるかテスト`() {
        val expected = 4
        val actual = Calculator.add(2, 2)
        assertEquals(expected, actual)
    }
}

object Calculator {
    fun add(a: Int, b: Int) = a + b
}
```

このコードは、2 + 2 が 4 と等しいことをテストします。

出力 (サンプル):
```
テスト成功 (緑色のチェックマーク)
```

## Deep Dive (深掘り)

JUnitはJavaの単体テストフレームワークですが、Kotlinでも幅広く使用されています。KotlinでのテストコードはJavaと非常によく似ていて、大きな変更を要しません。 他のテストフレームワークにはSpekやMockKなどがあります。JUnit 5では@DisplayNameアノテーションを使ってテストの説明をより読みやすくすることが可能です。

## See Also (関連リンク)

- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Spek Framework](https://www.spekframework.org/)
- [MockK Library](https://mockk.io/)
