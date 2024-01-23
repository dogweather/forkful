---
title:                "テストの作成"
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テストコードとは、プログラムが期待通りに動くことを保証するためのコードです。バグを早期に発見し、品質を確保するためにプログラマーはテストを書きます。

## How to: (やり方)
C++では、Google Testフレームワークを使ったテストが一般的です。以下に例を示します。

```C++
#include <gtest/gtest.h>

int Add(int a, int b) {
    return a + b;
}

TEST(MathTest, PositiveNumbers) {
    EXPECT_EQ(7, Add(3, 4));
}

TEST(MathTest, NegativeNumbers) {
    EXPECT_EQ(-5, Add(-2, -3));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

実行結果:
```
[==========] Running 2 tests from 1 test case.
[----------] Global test environment set-up.
[----------] 2 tests from MathTest
[ RUN      ] MathTest.PositiveNumbers
[       OK ] MathTest.PositiveNumbers (0 ms)
[ RUN      ] MathTest.NegativeNumbers
[       OK ] MathTest.NegativeNumbers (0 ms)
[----------] 2 tests from MathTest (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test case ran. (0 ms total)
[  PASSED  ] 2 tests.
```

## Deep Dive (深掘り)
テストコードの歴史は1990年代まで遡ります。Kent Beckらがテスト駆動開発(TDD)を提唱しました。代替としては、Boost.TestやCatch2などがあります。Google Testはテストケースや検証を簡単に書くためのマクロやユーティリティを提供します。

## See Also (関連情報)
Google Test公式ドキュメント:
https://github.com/google/googletest/blob/master/googletest/docs/primer.md

テスト駆動開発についてのさらなる情報:
http://agiledata.org/essays/tdd.html

Boost.Testドキュメント:
https://www.boost.org/doc/libs/release/libs/test/

Catch2 GitHubレポジトリ:
https://github.com/catchorg/Catch2
