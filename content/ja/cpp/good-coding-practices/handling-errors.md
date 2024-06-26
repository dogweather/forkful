---
date: 2024-01-26 00:50:29.322756-07:00
description: "\u65B9\u6CD5\uFF1A C++\u306F\u521D\u671F\u6BB5\u968E\u304B\u3089\u30A8\
  \u30E9\u30FC\u51E6\u7406\u3092\u6301\u3063\u3066\u3044\u307E\u3057\u305F\u3002\u6700\
  \u3082\u57FA\u672C\u7684\u306A\u5F62\u5F0F\u306F\u3001\u623B\u308A\u5024\u3092\u30C1\
  \u30A7\u30C3\u30AF\u3059\u308B\u3053\u3068\u3067\u3057\u305F\u3002\u53E4\u53C2\u306E\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u306A\u3089\u3001\u6A19\u6E96\u5316\u4EE5\u524D\u306E\
  C\u8A00\u8A9E\u306B\u30AF\u30E9\u30B9\u3092\u8FFD\u52A0\u3057\u305F\u6642\u4EE3\u3001\
  \u624B\u52D5\u3067\u306E\u30A8\u30E9\u30FC\u30C1\u30A7\u30C3\u30AF\u3092\u899A\u3048\
  \u3066\u3044\u308B\u3067\u3057\u3087\u3046\u3002\u2026"
lastmod: '2024-04-05T22:50:56.450836-06:00'
model: gpt-4-1106-preview
summary: "\u305D\u3057\u3066\u3001\u4E88\u671F\u3057\u306A\u3044\u554F\u984C\u3092\
  \u6271\u3046\u305F\u3081\u306E\u69CB\u9020\u5316\u3055\u308C\u305F\u65B9\u6CD5\u3068\
  \u3057\u3066\u3001C++\u306B\u4F8B\u5916\u51E6\u7406\u304C\u8FFD\u52A0\u3055\u308C\
  \u307E\u3057\u305F\u3002\u300Cthrow\u300D\u3067\u4F8B\u5916\u3092\u6295\u3052\u3001\
  `try/catch`\u3067\u6355\u307E\u3048\u307E\u3059\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## 方法：
例外を扱う基本的なtry-catchブロックは次の通りです：

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("おっと！何かがうまくいきませんでした。");
    } catch (const std::exception& e) {
        std::cerr << "エラー：" << e.what() << std::endl;
    }
    return 0;
}
```

サンプル出力：
```
エラー：おっと！何かがうまくいきませんでした。
```

## 深堀り
C++は初期段階からエラー処理を持っていました。最も基本的な形式は、戻り値をチェックすることでした。古参のプログラマなら、標準化以前のC言語にクラスを追加した時代、手動でのエラーチェックを覚えているでしょう。

そして、予期しない問題を扱うための構造化された方法として、C++に例外処理が追加されました。「throw」で例外を投げ、`try/catch`で捕まえます。

しばしば発生するエラーのタイプには、計算が間違っているような論理エラー、無効なメモリアドレスにアクセスするような実行時エラーがあります。例外は実行時エラーに最適です。論理エラーについては、アサーションやエラーコードを使用する方がしばしば適しています。

例外とエラーコードについては、現在も議論が続いています。例外は遅くなる可能性があり、複雑なコントロールフローを引き起こすかもしれません。エラーコードは、速いですが、コードがごちゃごちゃして保守が難しくなる可能性があります。トレードオフなので、使用する場合のコンテキストを知ることが重要です。

C++17では`std::optional`と`std::variant`が導入されました。これらは例外に代わる選択肢であり、有効な結果を返すかもしれないし返さないかもしれない関数に役立ちます。

例外安全性も別の頭痛の種です。それは例外にもかかわらずコードが提供する保証についてです。基本、強、nothrowの3つのレベルがあります。保証が多くなるほど、コードは複雑になるかもしれません。

最終的な考えとして、エラー処理は科学と同じくらい芸術です。それはあなたのアプリケーションが野生で生き残る方法を形作ります。例外を使い過ぎないでください。読みやすく、保守可能なコードを目指しましょう。

## 参考文献
- [cppreferenceの例外処理に関する記事](https://en.cppreference.com/w/cpp/language/exceptions)
- [Bjarne Stroustrup（ビャーネ・ストラウストラップ）のエラー処理に関する見解](http://www.stroustrup.com/except.pdf)
- [C++コアガイドラインの例外について](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
