---
date: 2024-01-27 20:32:49.274961-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u4E71\u6570\u3092\u751F\
  \u6210\u3059\u308B\u3053\u3068\u306F\u3001\u4E88\u6E2C\u53EF\u80FD\u306A\u9806\u5E8F\
  \u3084\u30D1\u30BF\u30FC\u30F3\u3092\u6301\u305F\u306A\u3044\u6570\u5B57\u306E\u5217\
  \u3092\u4F5C\u308A\u51FA\u3059\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4E88\u6E2C\u4E0D\u53EF\u80FD\u306A\
  \u51FA\u6765\u4E8B\u3092\u30B7\u30DF\u30E5\u30EC\u30FC\u30C8\u3057\u305F\u308A\u3001\
  \u30C6\u30B9\u30C8\u3084\u30C7\u30D0\u30C3\u30B0\u306B\u5229\u7528\u3057\u305F\u308A\
  \u3001\u516C\u5E73\u6027\u3084\u4E88\u6E2C\u4E0D\u53EF\u80FD\u6027\u3092\u4FDD\u8A3C\
  \u3059\u308B\u305F\u3081\u306E\u30B2\u30FC\u30E0\u30A2\u30EB\u30B4\u30EA\u30BA\u30E0\
  \u3067\u4F7F\u7528\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3001\u3053\u308C\
  \u3089\u306E\u6570\u5B57\u3092\u3088\u304F\u5229\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.545684-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u4E71\u6570\u3092\u751F\
  \u6210\u3059\u308B\u3053\u3068\u306F\u3001\u4E88\u6E2C\u53EF\u80FD\u306A\u9806\u5E8F\
  \u3084\u30D1\u30BF\u30FC\u30F3\u3092\u6301\u305F\u306A\u3044\u6570\u5B57\u306E\u5217\
  \u3092\u4F5C\u308A\u51FA\u3059\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4E88\u6E2C\u4E0D\u53EF\u80FD\u306A\
  \u51FA\u6765\u4E8B\u3092\u30B7\u30DF\u30E5\u30EC\u30FC\u30C8\u3057\u305F\u308A\u3001\
  \u30C6\u30B9\u30C8\u3084\u30C7\u30D0\u30C3\u30B0\u306B\u5229\u7528\u3057\u305F\u308A\
  \u3001\u516C\u5E73\u6027\u3084\u4E88\u6E2C\u4E0D\u53EF\u80FD\u6027\u3092\u4FDD\u8A3C\
  \u3059\u308B\u305F\u3081\u306E\u30B2\u30FC\u30E0\u30A2\u30EB\u30B4\u30EA\u30BA\u30E0\
  \u3067\u4F7F\u7528\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3001\u3053\u308C\
  \u3089\u306E\u6570\u5B57\u3092\u3088\u304F\u5229\u7528\u3057\u307E\u3059\u3002"
title: "\u4E71\u6570\u306E\u751F\u6210"
---

{{< edit_this_page >}}

## 何となぜ？

プログラミングで乱数を生成することは、予測可能な順序やパターンを持たない数字の列を作り出すことを意味します。プログラマーは、予測不可能な出来事をシミュレートしたり、テストやデバッグに利用したり、公平性や予測不可能性を保証するためのゲームアルゴリズムで使用したりするために、これらの数字をよく利用します。

## どのようにして：

C++で乱数を生成するには、`<random>`ヘッダーを使用するのが一般的です。これはC++11で導入され、さまざまな分布から乱数を生成するための広範な設備を提供します。

```C++
#include <iostream>
#include <random>

int main() {
    // 乱数エンジンの初期化
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // 範囲 [0, 99] を定義
    std::uniform_int_distribution<> distrib(0, 99); 

    // 定義された範囲内で5つの乱数を生成して表示
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

このコードサンプルは、`std::random_device`からシードを取得してMersenne Twister乱数ジェネレータを初期化します。次に、範囲 [0, 99] で一様整数分布を定義し、最終的にこの分布から5つの乱数を出力します。

サンプル出力はこのような形になりますが、実行ごとに異なる結果が得られることが予想されます：

```
45 67 32 23 88
```

## 深入り：

歴史的に、C++における乱数生成は、`rand()`関数やシード設定のための`srand()`関数に大きく依存していました。これらの関数は`<cstdlib>`ヘッダーに含まれています。しかし、このアプローチは生成された数の分布の一様性や予測可能性の欠如のために、たびたび批判されてきました。

C++11で`<random>`ヘッダーが導入されたことは、乱数を生成する方法において顕著な改善をもたらしました。このヘッダーは、乱数を生成するための洗練されたシステムを提供し、プログラマーの特定のニーズに合わせて組み合わせることができる様々なエンジン（`std::mt19937`のようなMersenne Twister用）や分布（整数の一様分布のための`std::uniform_int_distribution`のような）を提供しています。これにより、より予測可能な振る舞い、より良いパフォーマンス、そしてより大きな柔軟性を実現しています。

`<random>`ライブラリは古い`rand()`アプローチよりもはるかに優れていますが、特に暗号目的のために真にランダムな数を生成することは、追加の配慮が必要であるという点に注意する価値があります。暗号学的アプリケーションのためには、しばしばハードウェアのエントロピーソースを利用する、セキュリティに特化した設計のライブラリが代わりに使用されるべきです。
