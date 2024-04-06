---
date: 2024-01-20 17:38:05.459150-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.751709-06:00'
model: gpt-4-1106-preview
summary: "/ \u65B9\u6CD5 \u6B74\u53F2\u7684\u306BC++\u3067\u306F`<cctype>`\u30D8\u30C3\
  \u30C0\u306B\u3042\u308B`std::tolower`\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\
  \u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3057\u3066\u304D\u307E\u3057\
  \u305F\u3002\u3053\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u306FC\u8A00\u8A9E\u7531\u6765\
  \u3067\u3059\u3002C++11\u304B\u3089`std::transform`\u3092\u4F7F\u3046\u65B9\u6CD5\
  \u304C\u63A8\u5968\u3055\u308C\u307E\u3059\u3002\u3053\u308C\u306F\u7BC4\u56F2\u30D9\
  \u30FC\u30B9\u306E\u64CD\u4F5C\u3092\u5BB9\u6613\u306B\u3057\u3001\u30E9\u30E0\u30C0\
  \u5F0F\u306B\u3088\u308B\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3082\u53EF\u80FD\u306B\
  \u3057\u307E\u3059\u3002\u4EE3\u66FF\u624B\u6BB5\u3068\u3057\u3066\u3001\u81EA\u4F5C\
  \u306E\u30EB\u30FC\u30D7\u3067\u5909\u63DB\u3092\u884C\u3046\u3053\u3068\u3082\u3067\
  \u304D\u307E\u3059\u304C\u3001`std::transform`\u3068`std::tolower`\u3092\u7D44\u307F\
  \u5408\u308F\u305B\u308B\u65B9\u6CD5\u304C\u6700\u3082\u624B\u8EFD\u3067\u3059\u3002\
  \u30ED\u30B1\u30FC\u30EB\u306B\u57FA\u3065\u304F\u5909\u63DB\u306E\u305F\u3081`std::tolower`\u306F\
  \u30ED\u30B1\u30FC\u30EB\u306B\u654F\u611F\u306A\u30AA\u30FC\u30D0\u30FC\u30ED\u30FC\
  \u30C9\u3082\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u304C\u3001\u901A\u5E38\u306F\
  \u30C7\u30D5\u30A9\u30EB\u30C8\u306E\u300CC\u300D\u30ED\u30B1\u30FC\u30EB\u304C\u5229\
  \u7528\u3055\u308C\u307E\u3059\u3002"
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
