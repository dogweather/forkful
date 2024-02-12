---
title:                "複素数の扱い方"
date:                  2024-01-26T04:42:34.341830-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-complex-numbers.md"
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
