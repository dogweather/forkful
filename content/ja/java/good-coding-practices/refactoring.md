---
date: 2024-01-26 01:40:17.336462-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.959830-06:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u65E2\
  \u5B58\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30B3\u30FC\u30C9\u3092\u518D\u69CB\
  \u6210\u3059\u308B\u904E\u7A0B\u2014\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\
  \u5909\u66F4\u3059\u308B\u2014\u3053\u3068\u3067\u3042\u308A\u3001\u305D\u306E\u5916\
  \u90E8\u7684\u306A\u632F\u308B\u821E\u3044\u3092\u5909\u3048\u306A\u3044\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30BD\u30D5\u30C8\
  \u30A6\u30A7\u30A2\u306E\u975E\u6A5F\u80FD\u5C5E\u6027\u3092\u6539\u5584\u3059\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\u3064\u307E\u308A\
  \u3001\u8AAD\u307F\u3084\u3059\u3055\u3092\u9AD8\u3081\u3001\u8907\u96D1\u3055\u3092\
  \u6E1B\u3089\u3057\u3001\u30B3\u30FC\u30C9\u3092\u5C06\u6765\u306E\u4E8B\u696D\u306B\
  \u5411\u3051\u3066\u3088\u308A\u4FDD\u5B88\u3057\u3084\u3059\u304F\u3059\u308B\u305F\
  \u3081\u3067\u3059\u3002."
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

## 方法：
簡単なJavaクラスが、その貧弱な組織化と明確さの欠如のためにリファクタリングを求めているとしましょう。

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // その他の操作...
    }
}
```

リファクタリング後、私たちは以下のように持っています:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // その他の操作...
}
```

リファクタリングによって、メソッド名とパラメータを読みやすさのために改善し、シングルメソッド内の条件分岐の必要性をなくしました。各操作は現在、その目的を明確に述べています。

## 深堀り：
リファクタリングはSmalltalkコミュニティにそのルーツを持ち、コードの読みやすさとオブジェクト指向デザインを重視していますが、実際には90年代後半から00年代初頭にかけてJava界で大きく広まりました。特に、マーティン・ファウラーの画期的な書籍「リファクタリング：既存のコードのデザインを改善する」の出版後に顕著となりました。

リファクタリングには、ゼロからコードを書き直すなどの代替手段があります。しかし、リファクタリングがよく好まれるのは、アプリケーションの機能を混乱させずに、段階的な変更を伴うためです。

Java（または任意のプログラミング言語）でリファクタリングを行う際の実装の詳細は、コードスメル—コード内の深刻な問題の指標—を理解することを中心に展開します。長いメソッド、大きなクラス、重複するコード、プリミティブの過度な使用などがスメルに含まれます。メソッドの抽出、メソッドの移動、一時変数とクエリの置換などのリファクタリングパターンを適用することで、開発者はこれらのスメルを系統的に対処しつつ、常にコードが機能するように保つことができます。

自動化ツール、IntelliJ IDEAのリファクタリングサポートやEclipseのプラグインなど、変数、メソッド、クラスの名前の変更、メソッドや変数の抽出、メソッドやクラスを異なるパッケージや名前空間に移動するなどのリファクタリングを自動化することで、プロセスを支援することができます。

## 参照：
- マーティン・ファウラーの「リファクタリング：既存のコードのデザインを改善する」: https://martinfowler.com/books/refactoring.html
- Refactoring.Guruのリファクタリングテクニック: https://refactoring.guru/refactoring/techniques
- Eclipseでの自動リファクタリング: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- IntelliJ IDEAのリファクタリング機能: https://www.jetbrains.com/idea/features/refactoring.html

これらの資源は、リファクタリングの原則を理解するための基礎、またはこれらの原則を実践に活かすために利用できるツールのいずれかを提供します。
