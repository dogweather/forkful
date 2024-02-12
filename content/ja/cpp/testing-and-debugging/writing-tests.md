---
title:                "テストの作成"
aliases: - /ja/cpp/writing-tests.md
date:                  2024-02-03T19:30:06.251652-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
