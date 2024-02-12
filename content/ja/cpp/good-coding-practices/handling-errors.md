---
title:                "エラー処理"
aliases: - /ja/cpp/handling-errors.md
date:                  2024-01-26T00:50:29.322756-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/handling-errors.md"
---

{{< edit_this_page >}}

## 何となぜ？
エラー処理とは、物事がうまくいかないときの計画を立てることを意味します。プログラムがクラッシュを防ぎ、ソフトウェアを頑健でユーザーフレンドリーなものにするため、非常に重要です。

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
