---
date: 2024-01-20 17:55:45.713037-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.382485-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6700\u521D\u306EC\u8A00\u8A9E\u30D0\u30FC\u30B8\u30E7\u30F3\
  \u304B\u3089`main`\u95A2\u6570\u306F\u5F15\u6570\u3092\u53D7\u3051\u53D6\u308C\u308B\
  \u3088\u3046\u306B\u306A\u3063\u3066\u3044\u307E\u3057\u305F\u3002`argc`\u306F\u5F15\
  \u6570\u306E\u6570\u3092\u3001`argv`\u306F\u5F15\u6570\u306E\u914D\u5217\u3092\u6307\
  \u3059\u3002\u3053\u306E\u30B7\u30F3\u30D7\u30EB\u306A\u4ED5\u7D44\u307F\u306F\u3001\
  \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u304B\u3089\u306E\u5165\u529B\u3092\u67D4\
  \u8EDF\u306B\u6271\u3048\u308B\u3002\u4EE3\u66FF\u3068\u3057\u3066`getopt`\u95A2\
  \u6570\u3084\u30E9\u30A4\u30D6\u30E9\u30EA\u30FC\u3092\u4F7F\u3063\u305F\u308A\u3001\
  \u74B0\u5883\u5909\u6570\u3092\u8AAD\u3093\u3060\u308A\u3059\u308B\u65B9\u6CD5\u3082\
  \u3042\u308B\u304C\u3001\u57FA\u672C\u7684\u306A\u30A2\u30D7\u30ED\u30FC\u30C1\u306F\
  \u3053\u306E\u4E8C\u3064\u306E\u30D1\u30E9\u30E1\u30FC\u30BF\u306B\u3088\u308B\u3082\
  \u306E\u3067\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

## How to: (方法)
```C++
#include <iostream>

int main(int argc, char* argv[]) {
    std::cout << "プログラム名: " << argv[0] << std::endl;
    
    for (int i = 1; i < argc; ++i) {
        std::cout << "引数" << i << ": " << argv[i] << std::endl;
    }

    return 0;
}
```
実行例:
```
$ ./myprogram こんにちは 世界
プログラム名: ./myprogram
引数1: こんにちは
引数2: 世界
```

## Deep Dive (より深く)
最初のC言語バージョンから`main`関数は引数を受け取れるようになっていました。`argc`は引数の数を、`argv`は引数の配列を指す。このシンプルな仕組みは、コマンドラインからの入力を柔軟に扱える。代替として`getopt`関数やライブラリーを使ったり、環境変数を読んだりする方法もあるが、基本的なアプローチはこの二つのパラメータによるものです。

## See Also (関連情報)
- C++ reference on command line arguments: https://en.cppreference.com/w/cpp/language/main_function
- getopt manual page for argument parsing: https://man7.org/linux/man-pages/man3/getopt.3.html
- Boost.Program_options for sophisticated argument parsing: https://www.boost.org/doc/libs/release/doc/html/program_options.html
