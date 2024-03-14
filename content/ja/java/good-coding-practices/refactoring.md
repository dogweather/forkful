---
date: 2024-01-26 01:40:17.336462-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.959830-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングとは、既存のコンピュータコードを再構成する過程—ファクタリングを変更する—ことであり、その外部的な振る舞いを変えないことです。プログラマーは、ソフトウェアの非機能属性を改善するためにこれを行います。つまり、読みやすさを高め、複雑さを減らし、コードを将来の事業に向けてより保守しやすくするためです。

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
