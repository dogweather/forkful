---
title:    "C++: テストの書き方"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

プログラミングをする時、私たちはコードが予想通りに動作することを望んでいます。しかし、修正や新しい機能を追加する際には、予期しないバグが発生する可能性があります。このような場合、テストを書くことで予期せずにコードが崩れてしまうことを防ぐことができます。

## テストの書き方

テストを書くには、いくつかのステップが必要です。まずは、テストする機能やコードを理解することが重要です。次に、テストケースを作成し、実際にコードをテストしてみます。テストがすべて通過したら、テストを実行するためのコードを書きます。最後に、常にテストを実行して、コードに変更があった場合にはテストを更新するようにしましょう。

```C++
#include <iostream>
#include <cassert>

// 2つの数字を加算する関数
int add(int a, int b) {
    return a + b;
}

// テストケース
int main() {
    // テスト1: 正しい出力が得られることを確認
    assert(add(2, 3) == 5);
    // テスト2: 負の数でも計算が可能であることを確認
    assert(add(-2, 5) == 3);
    
    std::cout << "All tests passed!" << std::endl;
    
    return 0;
}
```

上記の例では、```assert()```を使用してテストを実行しています。この関数は、引数に与えられた条件が偽である場合にはプログラムを停止し、```assertion failed```というエラーメッセージを出力します。

## テストを深く掘り下げる

テストを書く際には、どのように入力値や条件を設定するかが重要です。入力値が異なる場合には、それぞれの場合に対応するテストを書く必要があります。また、コードのカバレッジを考えることも重要です。コードのすべての部分がテストされていることを確認しましょう。

## 参考リンク

- [Google C++ テスト フレームワーク - Google C++ スタイル ガイド](https://google.github.io/styleguide/cppguide.html#Google_C.2B-.2B-_Tests)
- [Google Test ユーザーガイド (日本語訳)](https://github.com/google/googletest/blob/master/googletest/docs/DocumentationJa.md)
- [Boost.Test ライブラリ ドキュメント(日本語)](https://www.boost.org/doc/libs/1_71_0/libs/test/doc/html/boost_test/oneminutetutorial.html)