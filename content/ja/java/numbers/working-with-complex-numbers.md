---
date: 2024-01-26 04:42:34.341830-07:00
description: "\u8907\u7D20\u6570\u306F\u3001\u865A\u6570\u5358\u4F4D `i`\uFF08`i^2\
  \ = -1`\uFF09\u306E\u8FFD\u52A0\u306B\u3088\u3063\u3066\u5B9F\u6570\u76F4\u7DDA\u3092\
  \u62E1\u5F35\u3057\u307E\u3059\u3002\u5DE5\u5B66\u3001\u7269\u7406\u5B66\u3001\u9AD8\
  \u5EA6\u306A\u6570\u5B66\u306A\u3069\u306E\u5206\u91CE\u3067\u975E\u5E38\u306B\u91CD\
  \u8981\u3067\u3042\u308A\u3001\u5B9F\u6570\u3067\u306F\u6271\u3046\u3053\u3068\u304C\
  \u3067\u304D\u306A\u3044\u96FB\u6D41\u3084\u4FE1\u53F7\u51E6\u7406\u306E\u3088\u3046\
  \u306A\u73FE\u8C61\u3092\u30E2\u30C7\u30EB\u5316\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.520078-06:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u3001\u865A\u6570\u5358\u4F4D `i`\uFF08`i^2 = -1`\uFF09\
  \u306E\u8FFD\u52A0\u306B\u3088\u3063\u3066\u5B9F\u6570\u76F4\u7DDA\u3092\u62E1\u5F35\
  \u3057\u307E\u3059\u3002\u5DE5\u5B66\u3001\u7269\u7406\u5B66\u3001\u9AD8\u5EA6\u306A\
  \u6570\u5B66\u306A\u3069\u306E\u5206\u91CE\u3067\u975E\u5E38\u306B\u91CD\u8981\u3067\
  \u3042\u308A\u3001\u5B9F\u6570\u3067\u306F\u6271\u3046\u3053\u3068\u304C\u3067\u304D\
  \u306A\u3044\u96FB\u6D41\u3084\u4FE1\u53F7\u51E6\u7406\u306E\u3088\u3046\u306A\u73FE\
  \u8C61\u3092\u30E2\u30C7\u30EB\u5316\u3057\u307E\u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？

複素数は、虚数単位 `i`（`i^2 = -1`）の追加によって実数直線を拡張します。工学、物理学、高度な数学などの分野で非常に重要であり、実数では扱うことができない電流や信号処理のような現象をモデル化します。

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
