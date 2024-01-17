---
title:                "部分文字列の抽出"
html_title:           "PHP: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何をするのか　そして　なぜ
抽出部分とは、文字列から特定の部分を切り取ることを指します。プログラマーがこれを行う理由は、大きな文字列をより小さな部分に分割することで、データをより効率的に処理することができるからです。

## 方法：
抽出部分を行うためのコード例と、```PHP ... ```コードブロック内のサンプル出力を示します。

抽出部分は、PHPのsubstr（）関数を使用して行うことができます。例えば、文字列「Hello World」から5文字目以降の部分を抽出するには、次のように記述します：

```
<?php
$string = "Hello World";
$substring = substr($string, 4);
echo $substring;
// 出力： "o World"
```

また、文字列から特定の場所から始まる部分を抽出することも可能です。例えば、文字列「Hello World」から2番目から5文字抽出するには、次のように記述します：

```
<?php
$string = "Hello World";
$substring = substr($string, 1, 5);
echo $substring;
// 出力： "ello"
```

## 深く掘り下げる：
抽出部分の方法は、初期のバージョンのPHPから使用されてきました。しかし、代替手段として、preg_match（）やstrpos（）といった関数も存在します。抽出部分を行う際の実装の詳細については、PHPのドキュメンテーションを参照することをお勧めします。

## 関連情報：
抽出部分に関するより詳しい情報は、PHPマニュアルやオンラインのソースコードリポジトリを参照することをお勧めします。また、抽出部分を行うためのより高度な専用ライブラリやパッケージも存在します。