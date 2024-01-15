---
title:                "テストの作成"
html_title:           "C++: テストの作成"
simple_title:         "テストの作成"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ

テストを書くことをやってみたいのか考えてみましょう。テストは、コードの機能を確認する手段です。コードが正しく実装されていることを保証することによって、開発プロセス全体をスムーズにすることができます。

## テストの書き方

テストを書くための基本的な方法を紹介します。

```C++
#include <iostream>
#include <cassert>

// 単純な関数の例
int multiply(int x, int y) {
    return x * y;
}

// テストの実行
int main() {
    // 期待結果の指定
    assert(multiply(2, 3) == 6); // 成功テスト - 問題なし
    assert(multiply(5, -5) == -25); // 成功テスト - 問題なし
    assert(multiply(9, 0) == 0); // 成功テスト - 問題なし
    assert(multiply(10, 10) == 101); // 失敗テスト - 期待結果と異なる
    assert(multiply(6, 9) == 42); // 失敗テスト - 期待結果と異なる

    std::cout << "All tests passed!"; // テストがすべて成功した場合に表示

    return 0;
}
```

出力結果:

```
All tests passed!
```

## ディープダイブ

テストを書くときに注意すべきポイントやテストの種類について詳しく見ていきましょう。

- テストは、コードの機能性のみをテストするものではありません。予期しないエラーやバグを見つけるための重要な手段でもあります。
- テストの種類には、ユニットテスト、結合テスト、機能テストなどがあります。それぞれのテストの目的や適用するタイミングを把握し、適切なテストを書くことが重要です。
- テストは頻繁に実行するべきです。コードに変更を加えるたびにテストを実行し、問題の早期発見を心がけましょう。
- テストを書く際には、コード内での依存関係を最小限に抑えることが重要です。これにより、テストの独立性が保たれ、問題を特定しやすくなります。

## さらに見る

- [Google Test](https://github.com/google/googletest) - C++でのテストフレームワークの一つ。
- [The Art of Unit Testing](https://www.amazon.co.jp/Art-Unit-Testing-examples/dp/1617290890) - ユニットテストの本。
- [CppUTest](https://cpputest.github.io/) - オープンソースのC / C++テストフレームワーク。