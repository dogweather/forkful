---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:06.251652-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.557308-06:00'
model: gpt-4-0125-preview
summary: "C++\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3068\u306F\u3001\u30B3\u30FC\
  \u30C9\u30D9\u30FC\u30B9\u306E\u30BB\u30AF\u30B7\u30E7\u30F3\u306E\u52D5\u4F5C\u3092\
  \u81EA\u52D5\u7684\u306B\u691C\u8A3C\u3059\u308B\u5C0F\u3055\u306A\u3001\u81EA\u5DF1\
  \u5B8C\u7D50\u578B\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u4F5C\u6210\u3059\u308B\
  \u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30B3\u30FC\u30C9\u304C\
  \u671F\u5F85\u901A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\
  \u3057\u3001\u30EA\u30B0\u30EC\u30C3\u30B7\u30E7\u30F3\uFF08\u3064\u307E\u308A\u3001\
  \u65B0\u3057\u3044\u5909\u66F4\u304C\u65E2\u5B58\u306E\u6A5F\u80FD\u3092\u58CA\u3059\
  \u3053\u3068\uFF09\u3092\u9632\u304E\u3001\u6642\u9593\u3092\u304B\u3051\u3066\u4FDD\
  \u5B88\u53EF\u80FD\u306A\u30B3\u30FC\u30C9\u30D9\u30FC\u30B9\u3092\u4FC3\u9032\u3057\
  \u307E\u3059\u3002."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

## 何となぜ?

C++でテストを書くとは、コードベースのセクションの動作を自動的に検証する小さな、自己完結型のプログラムを作成することを指します。プログラマーはこれを行うことで、コードが期待通りに動作することを確認し、リグレッション（つまり、新しい変更が既存の機能を壊すこと）を防ぎ、時間をかけて保守可能なコードベースを促進します。

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
