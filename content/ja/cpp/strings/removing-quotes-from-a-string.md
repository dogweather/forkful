---
date: 2024-01-26 03:38:09.135775-07:00
description: "\u65B9\u6CD5\uFF1A \u5F15\u7528\u7B26\u306F\u30B3\u30F3\u30D4\u30E5\u30FC\
  \u30C6\u30A3\u30F3\u30B0\u306E\u591C\u660E\u3051\u4EE5\u6765\u3001\u30C6\u30AD\u30B9\
  \u30C8\u306E\u5384\u4ECB\u3082\u306E\u3067\u3057\u305F\u3002\u6614\u3005\u306F\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u5F15\u7528\u7B26\u3092\u30D5\u30A3\u30EB\
  \u30BF\u30EA\u30F3\u30B0\u3059\u308B\u305F\u3081\u306B\u5404\u6587\u5B57\u3092\u4E00\
  \u3064\u305A\u3064\u30EB\u30FC\u30D7\u3059\u308B\u306E\u3092\u898B\u308B\u3053\u3068\
  \u304C\u3042\u308A\u307E\u3057\u305F\u3002\u4ECA\u65E5\u3067\u306F\u3001Standard\
  \ Template Library (STL)\u306E `std::remove`\u304C\u91CD\u3044\u4ED5\u4E8B\u3092\
  \u3057\u3066\u304F\u308C\u307E\u3059\u3002\u2026"
lastmod: '2024-04-05T22:50:56.425244-06:00'
model: gpt-4-0125-preview
summary: "\u4EE3\u66FF\u6848\uFF1F\u3082\u3061\u308D\u3093\u3067\u3059\uFF01`std::regex`\u3092\
  \u4F7F\u7528\u3057\u3066\u5F15\u7528\u7B26\u3092\u30BF\u30FC\u30B2\u30C3\u30C8\u306B\
  \u3059\u308B\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3082\
  \u3067\u304D\u307E\u3059\u304C\u3001\u3053\u308C\u306F\u5358\u7D14\u306A\u30BF\u30B9\
  \u30AF\u306B\u5BFE\u3057\u3066\u306F\u30AA\u30FC\u30D0\u30FC\u30AD\u30EB\u306B\u306A\
  \u308B\u53EF\u80FD\u6027\u306E\u3042\u308B\u3001\u4F7F\u7528\u3059\u308B\u306B\u306F\
  \u3042\u307E\u308A\u306B\u5F37\u529B\u306A\u65B9\u6CD5\u3067\u3059\u3002\u6700\u8FD1\
  \u306EC++\u306E\u30D5\u30EC\u30FC\u30D0\u30FC\u3092\u597D\u3080\u4EBA\u305F\u3061\
  \u306F\u3001\u975E\u5909\u66F4\u30A2\u30D7\u30ED\u30FC\u30C1\u7528\u306E `std::string_view`\u3092\
  \u8A66\u3057\u3066\u307F\u308B\u304B\u3082\u3057\u308C\u307E\u305B\u3093\u3002"
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
