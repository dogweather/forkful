---
title:                "数値の丸め処理"
aliases:
- /ja/php/rounding-numbers.md
date:                  2024-01-26T03:45:56.256953-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
数値を丸めるとは、小数点を定められた精度で切り捨てることであり、多くの場合は整数にします。プログラマーは計算を単純化するため、パフォーマンスを向上させるため、または出力をユーザーフレンドリーにするために数値を丸めます。

## 方法：
PHPには数値を丸めるいくつかの方法があります：`round()`, `ceil()`, `floor()`。それらの動作は以下の通りです：

```php
echo round(3.14159);   // 3 を返す
echo round(3.14159, 2); // 3.14 を返す

echo ceil(3.14159);    // 4 を返す、常に切り上げ

echo floor(3.14159);   // 3 を返す、常に切り下げ
```

## より詳しく
数値を丸めることは、古代の時代から数学と計算において不合理な無限小数を扱うために不可欠でした。PHPでは、`round()`は精度パラメータとモードを取り、その挙動に影響を与えます - `PHP_ROUND_HALF_UP`、`PHP_ROUND_HALF_DOWN`などは、".5"のシナリオに遭遇したときにどのように振る舞うかを定義します。金融アプリケーションでは精度が重要であり、丸めが法律で規制されている可能性があり、コード内での`round()`の実装方法に影響を与えることがあります。

組み込み関数の代替手段には、カスタム丸め方法や任意の精度算術のためのBC Math関数が含まれており、これらはより多くの制御を必要とするシナリオや、ネイティブの精度が不十分になる可能性がある非常に大きな数字を扱う場合に有用です。

## 参照
PHPマニュアルでさらに探求する：
- [PHP `round` 関数](https://php.net/manual/en/function.round.php)
- [PHP `ceil` 関数](https://php.net/manual/en/function.ceil.php)
- [PHP `floor` 関数](https://php.net/manual/en/function.floor.php)
- [任意の精度算術のためのBC Math](https://php.net/manual/en/book.bc.php)
