---
date: 2024-01-20 17:38:05.459150-07:00
description: "How to: / \u65B9\u6CD5 ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.532665-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
