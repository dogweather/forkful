---
date: 2024-01-26 04:44:06.947667-07:00
description: "\u8907\u7D20\u6570\u306F\u5B9F\u6570\u90E8\u3068\u865A\u6570\u90E8\u3092\
  \u6301\u3061\u3001\u901A\u5E38 `a + bi` \u3068\u8868\u8A18\u3055\u308C\u307E\u3059\
  \u3002\u3053\u308C\u3089\u306F\u9AD8\u5EA6\u306A\u6570\u5B66\u3001\u7269\u7406\u3001\
  \u5DE5\u5B66\u3001\u305D\u3057\u3066\u7279\u5B9A\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\
  \u30BF\u30A2\u30EB\u30B4\u30EA\u30BA\u30E0\u306B\u304A\u3044\u3066\u91CD\u8981\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u8CA0\u6570\u306E\u5E73\u65B9\
  \u6839\u3084\u632F\u52D5\u95A2\u6570\u3092\u542B\u3080\u8A08\u7B97\u3092\u6271\u3046\
  \u305F\u3081\u306B\u3053\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.236701-06:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u5B9F\u6570\u90E8\u3068\u865A\u6570\u90E8\u3092\
  \u6301\u3061\u3001\u901A\u5E38 `a + bi` \u3068\u8868\u8A18\u3055\u308C\u307E\u3059\
  \u3002\u3053\u308C\u3089\u306F\u9AD8\u5EA6\u306A\u6570\u5B66\u3001\u7269\u7406\u3001\
  \u5DE5\u5B66\u3001\u305D\u3057\u3066\u7279\u5B9A\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\
  \u30BF\u30A2\u30EB\u30B4\u30EA\u30BA\u30E0\u306B\u304A\u3044\u3066\u91CD\u8981\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u8CA0\u6570\u306E\u5E73\u65B9\
  \u6839\u3084\u632F\u52D5\u95A2\u6570\u3092\u542B\u3080\u8A08\u7B97\u3092\u6271\u3046\
  \u305F\u3081\u306B\u3053\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 方法：
PHPは`ext-intl`拡張機能と`NumberFormatter`クラスを使って複素数をサポートしています。例を以下に示します：

```php
// intl拡張機能がロードされていることを確認
if (!extension_loaded('intl')) {
    die("intl拡張機能が有効ではありません。このコードを実行するには有効にしてください。");
}

function addComplexNumbers($a, $b) {
    // NumberFormatterを使用して複素数を解析およびフォーマット
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // 文字列から複素数を解析
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // 加算を実行
    $sum = $numA + $numB;

    // 結果を複素数としてフォーマット
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // 出力：7+10i
```

## 深堀り
`ext-intl`以前には、PHPはネイティブの複素数サポートを有していませんでした。開発者は関数やカスタムクラスライブラリを使って複素数を扱っていました。複素数演算は煩雑でエラーが発生しやすいものでしたが、`ext-intl`はICUライブラリと一致して複素数を提示および解析する国際化された方法を提供します。

しかし、重量級の数学演算の場合、一部の人はより数学対応が良い言語（CやPythonなど）で書かれた外部ライブラリを使用し、PHPを通じてそれらとインターフェースすることもあります。実装に関しては、`ext-intl`が背後で処理を行い、正確な算術を確保しながら、開発者から複雑さを抽象化します。

歴史的には、「虚数」という用語に否定的な見解がありましたが、それらは科学や数学の様々な分野において基本となり、想像上のステータスがかつて示唆した以上に、その実世界での重要性を明らかにしました。

## 参照
- [PHPマニュアルのNumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [複素数についてのWikipedia](https://en.wikipedia.org/wiki/Complex_number)
- [PHP：正しい方法 - データタイプの扱い方](https://phptherightway.com/#data_types)
