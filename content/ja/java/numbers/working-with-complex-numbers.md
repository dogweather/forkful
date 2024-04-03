---
date: 2024-01-26 04:42:34.341830-07:00
description: "\u65B9\u6CD5: Java\u306B\u306F\u8907\u7D20\u6570\u3092\u30B5\u30DD\u30FC\
  \u30C8\u3059\u308B\u7D44\u307F\u8FBC\u307F\u306E\u6A5F\u80FD\u306F\u3042\u308A\u307E\
  \u305B\u3093\u304C\u3001\u81EA\u5206\u81EA\u8EAB\u306E\u30AF\u30E9\u30B9\u3092\u4F5C\
  \u6210\u3059\u308B\u304B\u3001\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\
  \u30B7\u30F3\u30D7\u30EB\u306A`ComplexNumber`\u30AF\u30E9\u30B9\u3092\u4F5C\u6210\
  \u3057\u3066\u4F7F\u7528\u3059\u308B\u305F\u3081\u306E\u7C21\u5358\u306A\u4F8B\u3092\
  \u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.937964-06:00'
model: gpt-4-0125-preview
summary: "Java\u306B\u306F\u8907\u7D20\u6570\u3092\u30B5\u30DD\u30FC\u30C8\u3059\u308B\
  \u7D44\u307F\u8FBC\u307F\u306E\u6A5F\u80FD\u306F\u3042\u308A\u307E\u305B\u3093\u304C\
  \u3001\u81EA\u5206\u81EA\u8EAB\u306E\u30AF\u30E9\u30B9\u3092\u4F5C\u6210\u3059\u308B\
  \u304B\u3001\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u30B7\u30F3\u30D7\
  \u30EB\u306A`ComplexNumber`\u30AF\u30E9\u30B9\u3092\u4F5C\u6210\u3057\u3066\u4F7F\
  \u7528\u3059\u308B\u305F\u3081\u306E\u7C21\u5358\u306A\u4F8B\u3092\u793A\u3057\u307E\
  \u3059\uFF1A."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 方法:
Javaには複素数をサポートする組み込みの機能はありませんが、自分自身のクラスを作成するか、ライブラリを使用することができます。ここでは、シンプルな`ComplexNumber`クラスを作成して使用するための簡単な例を示します：

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString to display complex numbers in a + bi form
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Quick test
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Sum: " + c1.add(c2));
    }
}
```

メインメソッドのサンプル出力は以下の通りです：

```
Sum: 3.0 + 7.0i
```

## 深掘り
Javaのような高水準言語が登場する前、プログラマーはFortranやCのような言語で数学ライブラリを直接使用して複雑な操作を管理していました。この概念は16世紀に遡り、Gerolamo CardanoやRafael Bombelliのような数学者によって功績があるとされています。

Javaでは、`java.lang.Math`が基本的なニーズに対するゴールドスタンダードですが、複素数はスキップしています。おそらく、すべてのプログラマーがそれらを使用するわけではないからでしょう。代替手段？ライブラリを使用します。Apache Commons Mathは、操作のための多くのメソッドを備えた`Complex`クラスを提供します。ただし、独自に作成することの利点は次のとおりです：軽量で、正確なニーズに合わせて調整可能、ライブラリのオーバーヘッドがありません。

重要な詳細の1つ：浮動小数点の精度に注意してください。コンピューターはいくつかの数値を正確に表現することができず、丸め誤差を引き起こします。繰り返し複合操作を実行するとき、これらの誤差が蓄積する可能性があります！

## 参照
より深い潜入とより複雑な操作については、以下をチェックしてください：

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [JScienceのComplexクラス](http://jscience.org/)
- Oracleのチュートリアル [浮動小数点算術](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
