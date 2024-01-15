---
title:                "文字列の長さを見つける"
html_title:           "PHP: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを知ることの重要性は理由がたくさんあります。例えば、ユーザーが入力したパスワードやメールアドレスが所定の文字数を満たしているかどうかをチェックしたり、文字列の加工をしたりする際に必要です。PHPでは、文字列の長さを素早く確認することができるため、プログラミングで頻繁に使用されます。

## 方法

PHPの```strlen()```関数を使用することで、文字列の長さを簡単に取得することができます。例えば、以下のように記述します。

```PHP
<?php
$str = "こんにちは、世界！";
echo strlen($str); // 出力結果：11
?>
```

変数```str```に文字列を代入し、```strlen()```関数で変数の長さを取得しています。出力結果は11となります。また、```mb_strlen()```関数を使用することで、マルチバイト文字列の場合にも正しい長さを取得することができます。

```PHP
<?php
$str = "こんにちは、世界！";
echo mb_strlen($str); // 出力結果：7
?>
```

```mb_strlen()```関数はマルチバイト対応のため、出力結果が異なります。

## ディープダイブ

文字列の長さを求める際には、注意すべき点があります。PHPでは、文字列に使用されている文字コードが異なる場合に、文字列の長さを正しく取得できないことがあります。これを解決する方法の1つは、```mb_internal_encoding()```関数を使用して文字コードの設定を行うことです。

```PHP
<?php
mb_internal_encoding("UTF-8");
$str = "こんにちは、世界！";
echo mb_strlen($str); // 出力結果：11
?>
```

このように、文字列の長さを取得する際には、文字コードにも注意しましょう。

## 関連記事

[PHPマニュアル：strlen()](https://www.php.net/manual/ja/function.strlen.php)

[PHPマニュアル：mb_strlen()](https://www.php.net/manual/ja/function.mb-strlen.php)

[PHPマニュアル：mb_internal_encoding()](https://www.php.net/manual/ja/function.mb-internal-encoding.php)