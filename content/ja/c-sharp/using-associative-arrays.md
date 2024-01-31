---
title:                "連想配列の使用"
date:                  2024-01-30T19:10:08.521861-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
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
