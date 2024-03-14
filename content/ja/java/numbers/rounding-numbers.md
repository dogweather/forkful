---
date: 2024-01-26 03:46:17.318195-07:00
description: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u6307\u5B9A\u3055\
  \u308C\u305F\u7CBE\u5EA6\u306E\u7A0B\u5EA6\u306B\u305D\u308C\u3089\u3092\u8ABF\u6574\
  \u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u304C\u305D\u308C\u3092\u884C\u3046\u306E\u306F\u3001\u6570\u5B57\
  \u3092\u8AAD\u307F\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u3001\u7279\u5B9A\u306E\
  \u4ED5\u69D8\u3092\u6E80\u305F\u3059\u305F\u3081\u3001\u307E\u305F\u306F\u7279\u5B9A\
  \u306E\u7BC4\u56F2\u5185\u3067\u8A08\u7B97\u3092\u5408\u308F\u305B\u308B\u305F\u3081\
  \uFF08\u4F8B\u3048\u3070\u3001\u6D6E\u52D5\u5C0F\u6570\u70B9\u6F14\u7B97\u306E\u7CBE\
  \u5EA6\u30A8\u30E9\u30FC\u3092\u907F\u3051\u308B\u306A\u3069\uFF09\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.939525-06:00'
model: gpt-4-0125-preview
summary: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u6307\u5B9A\u3055\
  \u308C\u305F\u7CBE\u5EA6\u306E\u7A0B\u5EA6\u306B\u305D\u308C\u3089\u3092\u8ABF\u6574\
  \u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u304C\u305D\u308C\u3092\u884C\u3046\u306E\u306F\u3001\u6570\u5B57\
  \u3092\u8AAD\u307F\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u3001\u7279\u5B9A\u306E\
  \u4ED5\u69D8\u3092\u6E80\u305F\u3059\u305F\u3081\u3001\u307E\u305F\u306F\u7279\u5B9A\
  \u306E\u7BC4\u56F2\u5185\u3067\u8A08\u7B97\u3092\u5408\u308F\u305B\u308B\u305F\u3081\
  \uFF08\u4F8B\u3048\u3070\u3001\u6D6E\u52D5\u5C0F\u6570\u70B9\u6F14\u7B97\u306E\u7CBE\
  \u5EA6\u30A8\u30E9\u30FC\u3092\u907F\u3051\u308B\u306A\u3069\uFF09\u3067\u3059\u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
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
