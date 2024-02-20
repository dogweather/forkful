---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:08.521861-07:00
description: "C#\u306B\u304A\u3051\u308B\u9023\u60F3\u914D\u5217\u3001\u307E\u305F\
  \u306F\u8F9E\u66F8\u306F\u3001\u30AD\u30FC\u3068\u5024\u306E\u30DA\u30A2\u3092\u4FDD\
  \u5B58\u30FB\u7BA1\u7406\u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\u307E\
  \u3059\u3002\u3053\u308C\u3089\u306F\u3001\u30E6\u30CB\u30FC\u30AF\u306A\u8B58\u5225\
  \u5B50\u306B\u57FA\u3065\u3044\u3066\u8FC5\u901F\u306B\u5024\u3092\u53D6\u5F97\u3059\
  \u308B\u5FC5\u8981\u304C\u3042\u308B\u6642\u3001\u3064\u307E\u308A\u8907\u96D1\u306A\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u306E\u30C7\u30FC\u30BF\u7BA1\
  \u7406\u3092\u5BB9\u6613\u306B\u3059\u308B\u969B\u306B\u9069\u3057\u3066\u3044\u307E\
  \u3059\u3002"
lastmod: 2024-02-19 22:05:01.261902
model: gpt-4-0125-preview
summary: "C#\u306B\u304A\u3051\u308B\u9023\u60F3\u914D\u5217\u3001\u307E\u305F\u306F\
  \u8F9E\u66F8\u306F\u3001\u30AD\u30FC\u3068\u5024\u306E\u30DA\u30A2\u3092\u4FDD\u5B58\
  \u30FB\u7BA1\u7406\u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\
  \u3002\u3053\u308C\u3089\u306F\u3001\u30E6\u30CB\u30FC\u30AF\u306A\u8B58\u5225\u5B50\
  \u306B\u57FA\u3065\u3044\u3066\u8FC5\u901F\u306B\u5024\u3092\u53D6\u5F97\u3059\u308B\
  \u5FC5\u8981\u304C\u3042\u308B\u6642\u3001\u3064\u307E\u308A\u8907\u96D1\u306A\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u306E\u30C7\u30FC\u30BF\u7BA1\u7406\
  \u3092\u5BB9\u6613\u306B\u3059\u308B\u969B\u306B\u9069\u3057\u3066\u3044\u307E\u3059\
  \u3002"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？

C#における連想配列、または辞書は、キーと値のペアを保存・管理することを可能にします。これらは、ユニークな識別子に基づいて迅速に値を取得する必要がある時、つまり複雑なアプリケーションでのデータ管理を容易にする際に適しています。

## 使い方:

C#では、`Dictionary<TKey, TValue>` クラスを使用して連想配列を扱います。始めるための簡単な例をこちらに示します:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // 辞書の作成
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // キーと値のペアを追加
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // キーを使用して値にアクセス
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // 値の更新
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Updated Apples: " + fruitBasket["Apples"]);
        
        // キーと値のペアを削除
        fruitBasket.Remove("Oranges");

        // 辞書を繰り返し処理
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
サンプル出力:
```
Apples: 5
Updated Apples: 7
Apples: 7
```

この例では、辞書の作成、要素の追加、アクセス、更新、削除、およびそれをイテレーションする方法を示しています。

## より深く

連想配列の概念は、PerlやPHPのようなスクリプト言語での使用にまで遡り、データのコレクションを管理するのに柔軟性を提供します。C#においては、`Dictionary<TKey, TValue>`が.NET Framework 2.0で導入された事実上の実装で、ハッシュテーブル内にデータを格納し、効率的な検索、追加、および削除を保証します。

しかし、辞書が非常に多様な用途に対応できるとはいえ、常に最良の選択肢であるとは限りません。順序付けされたコレクションを維持する場合、`SortedDictionary<TKey, TValue>`や`SortedList<TKey, TValue>`を検討するとよいでしょう。これらはソートされた順序を提供しますが、挿入と削除の操作が遅くなります。スレッドセーフを求めるシナリオでは、`ConcurrentDictionary<TKey, TValue>`がオーバーヘッドを追加しますが、手動のロックなしに複数のスレッドからの安全なアクセスを保証します。

最終的に、C#における連想配列の実装を選択することは、順序、性能、およびスレッドセーフに関する特定のニーズに依存します。
