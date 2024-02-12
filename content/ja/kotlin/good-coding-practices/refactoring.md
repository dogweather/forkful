---
title:                "リファクタリング"
aliases:
- /ja/kotlin/refactoring/
date:                  2024-01-26T01:44:07.521655-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/refactoring.md"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングは、コードの外部的な振る舞いを変えることなく、その構造、可読性、及びパフォーマンスを改善するために既存のコードを調整するプロセスです。プログラマーはコードをよりメンテナンスしやすくするため、新機能の追加を簡単にするため、及びバグをより簡単に見つけて修正するためにリファクタリングを行います。

## 方法：
ここに、一般的なコードの悪臭とそのリファクタリングされたバージョンを示すKotlinのスニペットがあります。多くの処理を行っているコードの塊から始めます：

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // 注文合計を計算
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // 割引を適用
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // さらに処理...
    }
}
```

より良い可読性と関心の分離のためにリファクタリングされた：

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

機能は変更していないのでサンプル出力はありませんが、コードの可読性とメンテナンス性が大きく向上しました！

## より深く
リファクタリングという概念はプログラミングが始まったときから存在していましたが、特に1990年代、マーティン・ファウラーが1999年に「Refactoring: Improving the Design of Existing Code（リファクタリング：既存のコードの設計を改善する）」を出版した後、一つの分野として真に隆盛を極めました。この本は実践に名前を与え、それを適用するための組織的な方法、リファクタリング技術のカタログを定義しました。

リファクタリングと代替手段の比較：ゼロからコードを書き直す（リスクが高く時間がかかる）、または単純に追加的な変更を行う（ソフトウェアの膨張と潜在的な技術的負債につながる）ことができます。リファクタリングは甘美なポイントに当たります。それは近代化し、リスクを低く保ちながらきれいにします。

実装においては、リファクタリングを始める前にプログラムの振る舞いを誤って変更しないようにするために、堅牢なテストセットを持つことが不可欠です。多くの現代のIDE（Kotlin用のIntelliJを含む）には、変数の名前を変更し、メソッドを抽出するなどの自動リファクタリングツールが備わっており、プロセスを速め、エラーを減らすことができます。

## 参照
- 「Refactoring: Improving the Design of Existing Code」マーティン・ファウラー著（このトピックに関する基礎作業）
- Kotlinのコーディング規約に関するドキュメント：[https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) （クリーンなコードの「Kotlinの方法」を理解するために）
- IntelliJ IDEAでのリファクタリングのサポートについてのJetBrains：[https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) （実践的なリファクタリングツール使用のために）
- より大きいリファクタリングの課題に取り組むためのGoogleのガイド：[https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html)
