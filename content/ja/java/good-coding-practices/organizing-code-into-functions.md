---
date: 2024-01-26 01:10:34.239291-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A \u3053\u3061\u3089\u304C\u5178\u578B\
  \u7684\u306A\u4F8B\u3067\u3059 \u2015 \u6570\u5024\u306E\u968E\u4E57\u3092\u8A08\
  \u7B97\u3059\u308B\u95A2\u6570\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.955443-06:00'
model: gpt-4-1106-preview
summary: "\u3053\u3061\u3089\u304C\u5178\u578B\u7684\u306A\u4F8B\u3067\u3059 \u2015\
  \ \u6570\u5024\u306E\u968E\u4E57\u3092\u8A08\u7B97\u3059\u308B\u95A2\u6570\u3067\
  \u3059."
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

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
