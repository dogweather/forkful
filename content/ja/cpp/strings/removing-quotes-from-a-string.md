---
date: 2024-01-26 03:38:09.135775-07:00
description: "\u65B9\u6CD5\uFF1A \u4EE5\u4E0B\u306FC++\u3067\u30AF\u30A9\u30FC\u30C8\
  \u3092\u6392\u9664\u3059\u308B\u7C21\u5358\u306A\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.349172-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 方法：
以下はC++でクォートを排除する簡単な方法です：

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

これを実行すると、以下が得られます：

```
Hello, World!
```

ほら！クォートが消えました。

## 詳細解説
引用符はコンピューティングの夜明け以来、テキストの厄介ものでした。昔々は、プログラマーが引用符をフィルタリングするために各文字を一つずつループするのを見ることがありました。今日では、Standard Template Library (STL)の `std::remove`が重い仕事をしてくれます。

代替案？もちろんです！`std::regex`を使用して引用符をターゲットにする正規表現を使用することもできますが、これは単純なタスクに対してはオーバーキルになる可能性のある、使用するにはあまりに強力な方法です。最近のC++のフレーバーを好む人たちは、非変更アプローチ用の `std::string_view`を試してみるかもしれません。

実装に関しては、`std::remove`はコンテナーから要素を実際には削除しないことを覚えておいてください；それは削除されなかった要素を前方へシャッフルし、範囲の新しい終端の先のイテレータを返します。だからこそ、望ましくない末尾を切り取るために `erase`メソッドが必要になります。

## 参照
- C++ `std::remove` リファレンス：[cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- `std::string` 操作についての詳細：[cplusplus.com](http://www.cplusplus.com/reference/string/string/)
