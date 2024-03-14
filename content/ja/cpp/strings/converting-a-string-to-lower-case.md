---
date: 2024-01-20 17:38:05.459150-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5185\u3067\u5168\u3066\u306E\u30A2\
  \u30EB\u30D5\u30A1\u30D9\u30C3\u30C8\u3092\u5C0F\u6587\u5B57\u306B\u3059\u308B\u64CD\
  \u4F5C\u306E\u3053\u3068\u3067\u3059\u3002\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u3001\
  \u6BD4\u8F03\u3092\u7C21\u5358\u306B\u884C\u3046\u3001\u307E\u305F\u306F\u30E6\u30FC\
  \u30B6\u30FC\u5165\u529B\u3092\u6A19\u6E96\u5316\u3059\u308B\u305F\u3081\u306B\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.532665-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5185\u3067\u5168\u3066\u306E\u30A2\
  \u30EB\u30D5\u30A1\u30D9\u30C3\u30C8\u3092\u5C0F\u6587\u5B57\u306B\u3059\u308B\u64CD\
  \u4F5C\u306E\u3053\u3068\u3067\u3059\u3002\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u3001\
  \u6BD4\u8F03\u3092\u7C21\u5358\u306B\u884C\u3046\u3001\u307E\u305F\u306F\u30E6\u30FC\
  \u30B6\u30FC\u5165\u529B\u3092\u6A19\u6E96\u5316\u3059\u308B\u305F\u3081\u306B\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
文字列を小文字に変換するとは、プログラム内で全てのアルファベットを小文字にする操作のことです。一貫性を保つ、比較を簡単に行う、またはユーザー入力を標準化するために、プログラマはこれを行います。

## How to: / 方法
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "こんにちは、C++ World!";
    std::transform(text.begin(), text.end(), text.begin(), 
        [](unsigned char c){ return std::tolower(c); });

    std::cout << text << std::endl;
    return 0;
}

// 出力: こんにちは、c++ world!
```

## Deep Dive / 深掘り
歴史的にC++では`<cctype>`ヘッダにある`std::tolower`関数を使用して文字を小文字に変換してきました。このアプローチはC言語由来です。C++11から`std::transform`を使う方法が推奨されます。これは範囲ベースの操作を容易にし、ラムダ式によるカスタマイズも可能にします。代替手段として、自作のループで変換を行うこともできますが、`std::transform`と`std::tolower`を組み合わせる方法が最も手軽です。ロケールに基づく変換のため`std::tolower`はロケールに敏感なオーバーロードも提供していますが、通常はデフォルトの「C」ロケールが利用されます。

## See Also / 関連情報
- C++ 言語参照: https://ja.cppreference.com/w/cpp/string/byte/tolower
- C++ `<algorithm>` ヘッダ: https://ja.cppreference.com/w/cpp/header/algorithm
- C++ Lambda 式: https://ja.cppreference.com/w/cpp/language/lambda
