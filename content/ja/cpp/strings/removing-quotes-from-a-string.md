---
date: 2024-01-26 03:38:09.135775-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u30AF\u30A9\u30FC\u30C8\u3092\u524A\u9664\
  \u3059\u308B\u3068\u3044\u3046\u306E\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u56F2\
  \u3093\u3067\u3044\u308B\u5384\u4ECB\u306A\u4E8C\u91CD\u307E\u305F\u306F\u5358\u4E00\
  \u306E\u6587\u5B57\uFF08'\u307E\u305F\u306F\"\uFF09\u3092\u53D6\u308A\u9664\u304F\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3053\u308C\u3092\u5165\u529B\u306E\u30B5\u30CB\u30BF\u30A4\u30BA\u3001\
  \u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u3078\u306E\u30C6\u30AD\u30B9\u30C8\u306E\u4FDD\
  \u5B58\u3001\u307E\u305F\u306F\u30AF\u30A9\u30FC\u30C6\u30FC\u30B7\u30E7\u30F3\u30DE\
  \u30FC\u30AF\u306E\u6563\u3089\u304B\u308A\u7269\u306A\u3057\u3067\u3055\u3089\u306A\
  \u308B\u51E6\u7406\u306E\u6E96\u5099\u306E\u305F\u3081\u306B\u3088\u304F\u884C\u3044\
  \u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.647890
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u30AF\u30A9\u30FC\u30C8\u3092\u524A\u9664\
  \u3059\u308B\u3068\u3044\u3046\u306E\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u56F2\
  \u3093\u3067\u3044\u308B\u5384\u4ECB\u306A\u4E8C\u91CD\u307E\u305F\u306F\u5358\u4E00\
  \u306E\u6587\u5B57\uFF08'\u307E\u305F\u306F\"\uFF09\u3092\u53D6\u308A\u9664\u304F\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3053\u308C\u3092\u5165\u529B\u306E\u30B5\u30CB\u30BF\u30A4\u30BA\u3001\
  \u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u3078\u306E\u30C6\u30AD\u30B9\u30C8\u306E\u4FDD\
  \u5B58\u3001\u307E\u305F\u306F\u30AF\u30A9\u30FC\u30C6\u30FC\u30B7\u30E7\u30F3\u30DE\
  \u30FC\u30AF\u306E\u6563\u3089\u304B\u308A\u7269\u306A\u3057\u3067\u3055\u3089\u306A\
  \u308B\u51E6\u7406\u306E\u6E96\u5099\u306E\u305F\u3081\u306B\u3088\u304F\u884C\u3044\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
文字列からクォートを削除するというのは、テキストを囲んでいる厄介な二重または単一の文字（'または"）を取り除くことを意味します。プログラマーはこれを入力のサニタイズ、データベースへのテキストの保存、またはクォーテーションマークの散らかり物なしでさらなる処理の準備のためによく行います。

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
