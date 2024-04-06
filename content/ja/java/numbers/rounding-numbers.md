---
date: 2024-01-26 03:46:17.318195-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Java\u306B\u306F\u6570\u5024\u3092\
  \u4E38\u3081\u308B\u305F\u3081\u306E\u8907\u6570\u306E\u65B9\u6CD5\u304C\u3042\u308A\
  \u307E\u3059\u3002`Math.round()`\u3001`BigDecimal`\u3001`DecimalFormat`\u3092\u4F7F\
  \u3063\u305F\u30AF\u30A4\u30C3\u30AF\u30C7\u30E2\u3092\u7D39\u4ECB\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-04-05T22:38:41.492827-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u3046\u3084\u3063\u3066\uFF1A Java\u306B\u306F\u6570\u5024\u3092\u4E38\
  \u3081\u308B\u305F\u3081\u306E\u8907\u6570\u306E\u65B9\u6CD5\u304C\u3042\u308A\u307E\
  \u3059\u3002`Math.round()`\u3001`BigDecimal`\u3001`DecimalFormat`\u3092\u4F7F\u3063\
  \u305F\u30AF\u30A4\u30C3\u30AF\u30C7\u30E2\u3092\u7D39\u4ECB\u3057\u307E\u3059\u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

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
