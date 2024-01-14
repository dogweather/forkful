---
title:    "C++: テストの書き方"
keywords: ["C++"]
---

{{< edit_this_page >}}

## なぜプログラマーはテストを書くべきか

プログラミングにおいて、テストは非常に重要な役割を果たします。テストを書くことで、コードの品質を保証し、エラーを事前に発見することができます。また、テストを書くことでコードをより柔軟に変更できるようになります。つまり、テストを書くことはプログラマーにとって不可欠な作業なのです。

## テストを書く方法

テストを書く方法は、プログラミング言語やフレームワークによって異なりますが、C++でのテストの書き方を紹介します。まず、テストを実行するためにはテスティングフレームワークが必要です。ここではGoogle Testを使用します。

```C++
#include <gtest/gtest.h>

// テストする関数
int add(int a, int b) {
    return a + b;
}

// テストケースを定義
TEST(TestAddFunction, PositiveNumbers) {
    // 期待する結果と実際の結果を比較
    EXPECT_EQ(add(3, 4), 7);
}

TEST(TestAddFunction, NegativeNumbers) {
    EXPECT_EQ(add(-7, -8), -15);
}

// メイン関数を定義
int main(int argc, char* argv[]) {
    // テストを実行
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

例として、add関数をテストするコードを書いています。テストケースでは期待する結果と実際の結果を比較し、結果が一致すればテストはパスします。これにより、プログラムのバグを早期に発見することができます。

## テストの詳細について

テストを書く際には、いくつかの注意点があります。まず、テストは単体で実行可能であることが重要です。また、どのような入力でもきちんと動作するように、ランダムな値や極端な値を使用してテストすることが大切です。さらに、テストのカバレッジを高くすることで、コードの全ての部分をテストすることができます。

## おすすめのリンク

- [Google Test公式サイト](https://github.com/google/googletest)
- [テスト駆動開発入門 | ドワンゴ Developers](https://dwango.github.io/articles/tdd)
- [プログラマーのためのテスト駆動開発 | Qiita](https://qiita.com/y-tsutsu/items/aa7e13c1ce1e6949e663)