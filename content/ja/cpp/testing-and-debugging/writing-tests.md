---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:06.251652-07:00
description: "\u65B9\u6CD5: C++\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u305F\u3081\
  \u306E\u6700\u3082\u4EBA\u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u306E1\u3064\u306FGoogle Test\u3067\u3059\u3002\
  \u307E\u305A\u3001Google Test\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u3001\
  \u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3068\u30EA\u30F3\u30AF\u3059\u308B\u5FC5\u8981\
  \u304C\u3042\u308A\u307E\u3059\u3002\u30BB\u30C3\u30C8\u30A2\u30C3\u30D7\u304C\u5B8C\
  \u4E86\u3057\u305F\u3089\u3001\u30C6\u30B9\u30C8\u30B1\u30FC\u30B9\u306E\u8A18\u8FF0\
  \u3092\u958B\u59CB\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:43.368173-06:00'
model: gpt-4-0125-preview
summary: "C++\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u305F\u3081\u306E\u6700\u3082\
  \u4EBA\u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306E1\u3064\u306FGoogle Test\u3067\u3059\u3002\u307E\u305A\u3001\
  Google Test\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u3001\u30D7\u30ED\u30B8\
  \u30A7\u30AF\u30C8\u3068\u30EA\u30F3\u30AF\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\
  \u307E\u3059\u3002\u30BB\u30C3\u30C8\u30A2\u30C3\u30D7\u304C\u5B8C\u4E86\u3057\u305F\
  \u3089\u3001\u30C6\u30B9\u30C8\u30B1\u30FC\u30B9\u306E\u8A18\u8FF0\u3092\u958B\u59CB\
  \u3067\u304D\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

## 方法:


### Google Test フレームワークを使用する
C++でテストを書くための最も人気のあるサードパーティライブラリの1つはGoogle Testです。まず、Google Testをインストールし、プロジェクトとリンクする必要があります。セットアップが完了したら、テストケースの記述を開始できます。

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

コードをファイルに保存し、g++コンパイラを使用してコンパイルし、Google Testライブラリとリンクします。すべてが正しく設定されていれば、生成された実行可能ファイルを実行するとテストが実行され、`add`関数が期待どおりに動作する場合は、次のようなものが表示されます:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### Catch2の使用
C++のもう1つの人気のあるテストフレームワークはCatch2です。よりシンプルな構文を持っており、通常はライブラリに対してリンクする必要がありません（ヘッダーのみ）。Catch2でシンプルなテストを書く方法の例をこちらに示します:

```cpp
#define CATCH_CONFIG_MAIN  // これはCatchにmain()を提供するように指示する - これは1つのcppファイルでのみ行う
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Integers are multiplied", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

このテストをコンパイルして実行すると、Catch2はテストが合格したか失敗したかを明確に指示する出力を提供し、失敗をデバッグするために必要な情報を提供します:

```
===============================================================================
All tests passed (1 assertion in 1 test case)
```

これらの例は、テストフレームワークをC++開発ワークフローに統合することで、コードの信頼性と保守性を大幅に向上させることができることを示しています。
