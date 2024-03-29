---
date: 2024-01-26 01:10:34.239291-07:00
description: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u307E\u3068\u3081\u308B\u3068\
  \u3044\u3046\u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3068\u3044\u3046\
  \u5927\u304D\u306A\u584A\u3092\u3001\u305D\u308C\u305E\u308C\u304C\u660E\u78BA\u306A\
  \u30BF\u30B9\u30AF\u3092\u6301\u3064\u7BA1\u7406\u3057\u3084\u3059\u3044\u30C1\u30E3\
  \u30F3\u30AF\u306B\u5206\u5272\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u3092\
  \u8AAD\u307F\u3084\u3059\u304F\u3001\u518D\u5229\u7528\u53EF\u80FD\u3067\u3001\u4FDD\
  \u5B88\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.955443-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u307E\u3068\u3081\u308B\u3068\
  \u3044\u3046\u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3068\u3044\u3046\
  \u5927\u304D\u306A\u584A\u3092\u3001\u305D\u308C\u305E\u308C\u304C\u660E\u78BA\u306A\
  \u30BF\u30B9\u30AF\u3092\u6301\u3064\u7BA1\u7406\u3057\u3084\u3059\u3044\u30C1\u30E3\
  \u30F3\u30AF\u306B\u5206\u5272\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u3092\
  \u8AAD\u307F\u3084\u3059\u304F\u3001\u518D\u5229\u7528\u53EF\u80FD\u3067\u3001\u4FDD\
  \u5B88\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
---

{{< edit_this_page >}}

## なにを？ なぜ？
コードを関数にまとめるということは、プログラムという大きな塊を、それぞれが明確なタスクを持つ管理しやすいチャンクに分割することを意味します。プログラマーは、コードを読みやすく、再利用可能で、保守しやすくするためにこれを行います。

## どのように：
こちらが典型的な例です ― 数値の階乗を計算する関数です。

```java
public class MathUtils {

    public static void main(String[] args) {
        int number = 5;
        int result = factorial(number);
        System.out.println(number + " の階乗は: " + result);
    }
    
    public static int factorial(int n) {
        if (n <= 1) {
            return 1;
        }
        return n * factorial(n - 1);
    }
}
```

出力は以下の通りです：
```
5 の階乗は: 120
```

## 詳細解説
関数が登場する前は、コードは一つの巨大なブロックにぎゅっと詰め込まれており、デバッグ作業は干し草の中から針を見つけるようなものでした。今では、機能を関数にカプセル化することで、問題を迅速に切り分けることができます。代わりに使えるものとしては、Java のラムダ式や、オブジェクト指向プログラミングのメソッドがあり、どちらも似たような目的で使われます。関数を書くときには、以下の点を覚えておいてください：(1) 各関数には単一の責任があること、そして (2) 関数名はその目的をはっきりと表すべきであること。

## 関連情報
コードの整理についての詳細は：
- ロバート・C・マーティン著『クリーンコード』
- マーティン・ファウラー著『リファクタリング: 既存のコードの設計を改善する』
- [オラクルJavaドキュメントのメソッド定義](https://docs.oracle.com/javase/tutorial/java/javaOO/methods.html)
