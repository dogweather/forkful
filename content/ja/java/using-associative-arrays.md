---
title:                "連想配列の使用"
date:                  2024-01-30T19:11:48.553114-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"
programming_language: "Java"
category:             "Java"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Javaでは、連想配列またはマップを使用すると、キーと値のペアを効率的にデータ検索や操作のために保存できます。プログラマーは、アイテムの出現回数を数えたり、ユーザーをそれらの権限にマッピングしたりするようなタスクにこれらを使用します。なぜなら、それらは高速なアクセスと更新を提供するからです。

## 方法:

Javaにはいくつかの言語のような組み込みの連想配列はありませんが、`Map`インターフェイスや`HashMap`、`TreeMap`のようなクラスを提供して、その役割を果たします。`HashMap`の使用方法は以下の通りです：

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // HashMapの作成
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // 要素の追加
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // 要素へのアクセス
        System.out.println("Aliceの年齢: " + ageOfFriends.get("Alice"));
        
        // 存在しないキーの処理
        System.out.println("マップにいない誰かの年齢: " + ageOfFriends.getOrDefault("Dan", -1));

        // 要素のイテレーション
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + "は" + entry.getValue() + "歳です。");
        }
    }
}
```

サンプル出力：

```
Aliceの年齢: 24
マップにいない誰かの年齢: -1
Aliceは24歳です。
Bobは30歳です。
Charlieは28歳です。
```

`HashMap`は実装の一つに過ぎません。キーがユニークでそれらをソートされた状態で必要とする場合は、`TreeMap`を検討してください。挿入された順序を保持するマップが必要であれば、`LinkedHashMap`が良いでしょう。

## 深掘り

Javaのマップは、JDK 1.2で導入されたコレクションフレームワークの一部ですが、エントリをより簡単にイテレートするための`forEach`メソッドの導入を含む、年々、大きな改善を見てきました。マップ実装(`HashMap`、`LinkedHashMap`、`TreeMap`)の選択は、注文と性能の観点から具体的なニーズによって決定されるべきです。たとえば、`HashMap`は基本操作（getとput）に対してO(1)時間性能を提供しますが、ハッシュ関数が要素をバケット間で適切に分散させると仮定しています。しかし、自然順序付けまたはカスタムコンパレータに基づくソートが必要な場合、`TreeMap`が挿入とルックアップのためのO(log n)時間を提供する、行く先です。

`Map`が導入される前は、連想配列は通常、2つの並列配列（1つはキー用、もう1つは値用）または効率の低いカスタムデータ構造を使用して実装されていました。`Map`及びその実装に代わる現在の選択肢には、値によって効率的にキーを見つける必要がある場合などに特殊マップを提供するサードパーティライブラリ（GoogleのGuavaライブラリのBiMapなど）が含まれる場合があります。しかし、Javaでのほとんどの使用例では、標準ライブラリのマップがタスクを処理するのに十分強力で柔軟です。
