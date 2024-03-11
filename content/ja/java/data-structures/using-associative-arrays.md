---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:48.553114-07:00
description: "Java\u3067\u306F\u3001\u9023\u60F3\u914D\u5217\u307E\u305F\u306F\u30DE\
  \u30C3\u30D7\u3092\u4F7F\u7528\u3059\u308B\u3068\u3001\u30AD\u30FC\u3068\u5024\u306E\
  \u30DA\u30A2\u3092\u52B9\u7387\u7684\u306B\u30C7\u30FC\u30BF\u691C\u7D22\u3084\u64CD\
  \u4F5C\u306E\u305F\u3081\u306B\u4FDD\u5B58\u3067\u304D\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A2\u30A4\u30C6\u30E0\u306E\u51FA\u73FE\u56DE\
  \u6570\u3092\u6570\u3048\u305F\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u3092\u305D\u308C\
  \u3089\u306E\u6A29\u9650\u306B\u30DE\u30C3\u30D4\u30F3\u30B0\u3057\u305F\u308A\u3059\
  \u308B\u3088\u3046\u306A\u30BF\u30B9\u30AF\u306B\u3053\u308C\u3089\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002\u306A\u305C\u306A\u3089\u3001\u305D\u308C\u3089\u306F\u9AD8\
  \u901F\u306A\u30A2\u30AF\u30BB\u30B9\u3068\u66F4\u65B0\u3092\u63D0\u4F9B\u3059\u308B\
  \u304B\u3089\u3067\u3059\u3002"
lastmod: '2024-03-11T00:14:15.518617-06:00'
model: gpt-4-0125-preview
summary: "Java\u3067\u306F\u3001\u9023\u60F3\u914D\u5217\u307E\u305F\u306F\u30DE\u30C3\
  \u30D7\u3092\u4F7F\u7528\u3059\u308B\u3068\u3001\u30AD\u30FC\u3068\u5024\u306E\u30DA\
  \u30A2\u3092\u52B9\u7387\u7684\u306B\u30C7\u30FC\u30BF\u691C\u7D22\u3084\u64CD\u4F5C\
  \u306E\u305F\u3081\u306B\u4FDD\u5B58\u3067\u304D\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30A2\u30A4\u30C6\u30E0\u306E\u51FA\u73FE\u56DE\u6570\
  \u3092\u6570\u3048\u305F\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u3092\u305D\u308C\u3089\
  \u306E\u6A29\u9650\u306B\u30DE\u30C3\u30D4\u30F3\u30B0\u3057\u305F\u308A\u3059\u308B\
  \u3088\u3046\u306A\u30BF\u30B9\u30AF\u306B\u3053\u308C\u3089\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002\u306A\u305C\u306A\u3089\u3001\u305D\u308C\u3089\u306F\u9AD8\u901F\
  \u306A\u30A2\u30AF\u30BB\u30B9\u3068\u66F4\u65B0\u3092\u63D0\u4F9B\u3059\u308B\u304B\
  \u3089\u3067\u3059\u3002"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
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
