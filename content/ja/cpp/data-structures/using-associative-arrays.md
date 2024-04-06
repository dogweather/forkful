---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:55.916591-07:00
description: "\u65B9\u6CD5: C++ \u3067\u306F\u3001`<map>` \u3068 `<unordered_map>`\
  \ \u306E\u30D8\u30C3\u30C0\u30D5\u30A1\u30A4\u30EB\u3068\u3068\u3082\u306B\u9023\
  \u60F3\u914D\u5217\u304C\u4F7F\u3048\u308B\u3088\u3046\u306B\u306A\u308A\u307E\u3059\
  \u3002\u4F8B\u3092\u6319\u3052\u306A\u304C\u3089\u3001\u305D\u306E\u4F7F\u7528\u65B9\
  \u6CD5\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002 `std::map` \u306F\u30AD\
  \u30FC\u306B\u57FA\u3065\u3044\u3066\u8981\u7D20\u3092\u30BD\u30FC\u30C8\u3055\u308C\
  \u305F\u72B6\u614B\u3067\u4FDD\u6301\u3057\u307E\u3059\u3002\u4F7F\u3044\u59CB\u3081\
  \u308B\u306B\u306F\u4EE5\u4E0B\u306E\u3088\u3046\u306B\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.541525-06:00'
model: gpt-4-0125-preview
summary: "C++ \u3067\u306F\u3001`<map>` \u3068 `<unordered_map>` \u306E\u30D8\u30C3\
  \u30C0\u30D5\u30A1\u30A4\u30EB\u3068\u3068\u3082\u306B\u9023\u60F3\u914D\u5217\u304C\
  \u4F7F\u3048\u308B\u3088\u3046\u306B\u306A\u308A\u307E\u3059\u3002\u4F8B\u3092\u6319\
  \u3052\u306A\u304C\u3089\u3001\u305D\u306E\u4F7F\u7528\u65B9\u6CD5\u3092\u898B\u3066\
  \u307F\u307E\u3057\u3087\u3046."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 方法:
C++ では、`<map>` と `<unordered_map>` のヘッダファイルとともに連想配列が使えるようになります。例を挙げながら、その使用方法を見てみましょう。

### `std::map` の使用
`std::map` はキーに基づいて要素をソートされた状態で保持します。使い始めるには以下のようにします：

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // 値の挿入
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // 値へのアクセス
    std::cout << "Bob の年齢: " << ageMap["Bob"] << std::endl;
    
    // map のイテレート
    for(const auto &pair : ageMap) {
        std::cout << pair.first << " の年齢は " << pair.second << " 歳です。" << std::endl;
    }
    
    return 0;
}
```

### `std::unordered_map` の使用
順序は関係なく、パフォーマンスが重要な場合は、`std::unordered_map` が便利です。挿入、検索、削除の平均的な複雑さで高速です。

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // 値の挿入
    productPrice["milk"] = 2.99;
    productPrice["bread"] = 1.99;
    
    // 値へのアクセス
    std::cout << "ミルクの価格: $" << productPrice["milk"] << std::endl;
    
    // unordered_map のイテレート
    for(const auto &pair : productPrice) {
        std::cout << pair.first << " のコストは $" << pair.second << " です。" << std::endl;
    }
    
    return 0;
}
```

## 深掘り
C++ での連想配列、特に `std::map` と `std::unordered_map` は、要素を保存するだけではありません。検索、挿入、削除といった操作を効果的な時間の複雑さ（`std::map` の場合は対数的、`std::unordered_map` の場合は平均的に定数時間）で可能にすることで、より複雑なデータ管理の基礎を提供します。この効率性は、`std::map` にはバランスのとれた木、`std::unordered_map` にはハッシュテーブルという、背後にあるデータ構造から来ています。

歴史的に、これらが標準ライブラリの一部となる前は、プログラマーは自分たちでバージョンを実装するか、サードパーティのライブラリを使用しなければならず、一貫性の欠如や潜在的な非効率性につながりました。C++ の標準ライブラリにマップが含まれたことで、その使用が標準化されただけでなく、異なるコンパイラーやプラットフォーム間でのパフォーマンスも最適化されました。

どちらも強力ではありますが、`std::map` と `std::unordered_map` の選択は、使用ケースの詳細にかかっています。順序付けられたデータが必要で、若干のパフォーマンスのトレードオフを気にしない場合は、`std::map` を選びます。スピードを求め、順序を気にしない場合は、`std::unordered_map` がおそらくより良い選択です。

しかし、複雑なデータ構造を扱う場合、常にトレードオフが存在することに注意が必要です。ニッチなケースでは、他のデータ構造やサードパーティのライブラリが、特定のニーズに合ったより良いパフォーマンスや機能を提供するかもしれません。プロジェクトの要件に基づいて常にオプションを考えてください。
