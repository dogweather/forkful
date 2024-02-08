---
title:                "コードを関数に整理する"
aliases:
- ja/java/organizing-code-into-functions.md
date:                  2024-01-26T01:10:34.239291-07:00
model:                 gpt-4-1106-preview
simple_title:         "コードを関数に整理する"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/organizing-code-into-functions.md"
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
