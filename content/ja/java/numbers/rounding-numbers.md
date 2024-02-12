---
title:                "数値の丸め処理"
aliases:
- /ja/java/rounding-numbers.md
date:                  2024-01-26T03:46:17.318195-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
数値を丸めるとは、指定された精度の程度にそれらを調整することを意味します。プログラマーがそれを行うのは、数字を読みやすくするため、特定の仕様を満たすため、または特定の範囲内で計算を合わせるため（例えば、浮動小数点演算の精度エラーを避けるなど）です。

## どうやって：
Javaには数値を丸めるための複数の方法があります。`Math.round()`、`BigDecimal`、`DecimalFormat`を使ったクイックデモを紹介します。

```java
public class RoundingDemo {
    public static void main(String[] args) {
        double num = 123.4567;

        // Math.round()を使用
        long roundedNum = Math.round(num);
        System.out.println(roundedNum); // 出力: 123

        // より細かい制御のためにBigDecimalを使用
        BigDecimal bd = new BigDecimal(num).setScale(2, RoundingMode.HALF_UP);
        double roundedBigDecimal = bd.doubleValue();
        System.out.println(roundedBigDecimal); // 出力: 123.46

        // DecimalFormatの使用
        DecimalFormat df = new DecimalFormat("#.##");
        String formattedNum = df.format(num);
        System.out.println(formattedNum); // 出力: 123.46
    }
}
```

## 詳細な解説
歴史的に、数値を丸めることはアナログ計算に不可欠であり、効率性と正確性のためにデジタル計算に持ち越されています。浮動小数点演算からの丸め誤差のように、これは取るに足らない問題ではありません。それらは、たとえば、航空宇宙や金融アプリケーションで、計算を蓄積的に台無しにする可能性があります。

`Math.round()`を超えて、`BigDecimal`を使えば、スケールと丸めモードをより細かく制御できます。また、`DecimalFormat`はテキスト出力のフォーマットの一部として数値を丸める必要がある場合に使用します。丸めの代替手段には、床関数、天井関数、切り捨てがあり、これらは通常、さまざまな`Math`メソッドによって処理されます。

使用例に応じて、丸め戦略は変わるかもしれません。たとえば、精度が重要な金融計算には`BigDecimal`が最適です。一方、`Math.round()`は、丸めモードにあまりこだわらない一般的な操作に対する素早い方法です。

## 参照
- [OracleのJava Mathドキュメント](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Math.html)
- [浮動小数点演算のIEEE標準（IEEE 754）](https://ieeexplore.ieee.org/document/4610935)
- [JavaのDecimalFormatクラス](https://docs.oracle.com/javase/7/docs/api/java/text/DecimalFormat.html)
