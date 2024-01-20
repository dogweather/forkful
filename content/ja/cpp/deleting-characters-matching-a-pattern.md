---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何と何故？

**パターンに一致する文字を削除する**とは、ある特定のパターンに一致する全ての文字を取り除くプログラミング手法です。これは、不必要な文字を消去することでデータの精度を上げるため、プログラマーによく用いられます。

## どうやって：

以下に、パターンに一致する文字を削除する簡単なC++コードを示します。

```C++
#include <algorithm>
#include <string>

int main() {
    std::string str = "abc123abc123";
    char c = 'a';

    str.erase(std::remove(str.begin(), str.end(), c), str.end());

    return 0;
}
```
上記のコードでは、文字列 "abc123abc123" から 'a' と一致する全ての文字を削除しています。

## ディープダイブ：

文字列から特定のパターンに一致する文字を削除するという考え方は、プログラミングの初期から存在しています。C++では、`std::remove`関数と`erase`メソッドの組み合わせによりこれを実現します。ただし、他のアプローチもあります。

例えば、正規表現を使用してパターンに一致する文字列を探し出すことも可能です。その場合、見つかった文字列を別の文字列で置き換えたり、そのまま削除したりすることができます。

削除操作は、元の文字列を変更するため、性能面で注意が必要です。大きなデータセットで大量の削除操作を行うと、プログラムのパフォーマンスに影響を及ぼすことがあります。

## 参考リンク：

1. [C++ 文字列](https://ja.cppreference.com/w/cpp/string)
2. [std::remove](https://en.cppreference.com/w/cpp/algorithm/remove)
3. [std::erase](https://en.cppreference.com/w/cpp/string/basic_string/erase)