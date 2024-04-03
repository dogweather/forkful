---
date: 2024-01-26 01:40:17.336462-07:00
description: "\u65B9\u6CD5\uFF1A \u7C21\u5358\u306AJava\u30AF\u30E9\u30B9\u304C\u3001\
  \u305D\u306E\u8CA7\u5F31\u306A\u7D44\u7E54\u5316\u3068\u660E\u78BA\u3055\u306E\u6B20\
  \u5982\u306E\u305F\u3081\u306B\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\
  \u6C42\u3081\u3066\u3044\u308B\u3068\u3057\u307E\u3057\u3087\u3046\u3002"
lastmod: '2024-03-13T22:44:41.959830-06:00'
model: gpt-4-0125-preview
summary: "\u7C21\u5358\u306AJava\u30AF\u30E9\u30B9\u304C\u3001\u305D\u306E\u8CA7\u5F31\
  \u306A\u7D44\u7E54\u5316\u3068\u660E\u78BA\u3055\u306E\u6B20\u5982\u306E\u305F\u3081\
  \u306B\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u6C42\u3081\u3066\u3044\
  \u308B\u3068\u3057\u307E\u3057\u3087\u3046."
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
