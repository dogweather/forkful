---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:08.521861-07:00
description: "\u4F7F\u3044\u65B9: C#\u3067\u306F\u3001`Dictionary<TKey, TValue>` \u30AF\
  \u30E9\u30B9\u3092\u4F7F\u7528\u3057\u3066\u9023\u60F3\u914D\u5217\u3092\u6271\u3044\
  \u307E\u3059\u3002\u59CB\u3081\u308B\u305F\u3081\u306E\u7C21\u5358\u306A\u4F8B\u3092\
  \u3053\u3061\u3089\u306B\u793A\u3057\u307E\u3059."
lastmod: '2024-03-13T22:44:42.112349-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u306F\u3001`Dictionary<TKey, TValue>` \u30AF\u30E9\u30B9\u3092\u4F7F\
  \u7528\u3057\u3066\u9023\u60F3\u914D\u5217\u3092\u6271\u3044\u307E\u3059\u3002\u59CB\
  \u3081\u308B\u305F\u3081\u306E\u7C21\u5358\u306A\u4F8B\u3092\u3053\u3061\u3089\u306B\
  \u793A\u3057\u307E\u3059."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

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
