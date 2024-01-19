---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)
文字列を補間するとは、文字列内に変数を直接埋め込むことを示します。プログラマーがこれを行う主要な理由は、コードの読みやすさと効率を向上させるためです。

## やり方 (How to:)
以下に文字列補間の基本的な使用方法の例を示します:

```PHP
$name = "John";
echo "Hello, $name";
```
このコードは "Hello, John"と出力します。 

深いダイブ (Deep Dive)
文字列の補間は、Perlなどの他の多くのスクリプト言語からPHPに借用された概念です。これには代替案もありますが、主に単純リテラル文字列または複合（ビルディング）文字列があります。
単純なリテラル文字列は次のようになります:

```PHP
$name = "John";
echo "Hello, " . $name;
```
このコードも同じく "Hello, John"と出力します。

補間は内部的に文字列の連結を行うため、結果として得られる文字列が特に長い場合や大量の変数を含む場合、パフォーマンスに影響を及ぼす可能性があることに注意が必要です。

## 参照先 (See Also)
 * PHP: 文字列 - Manual: [PHP: Strings - Manual](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)
 * PHPの文字列補間について: [Understanding PHP String Interpolation](https://www.digitalocean.com/community/tutorials/understanding-php-string-interpolation)