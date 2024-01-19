---
title:                "文字列を大文字にする"
html_title:           "PHP: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なに？なぜ？
文字列の大文字化とは、全ての文字を大文字に変換するプロセスのことを指します。プログラマーがこれを行う主な理由は、ユーザーの入力形式を統一したい時や、特定の文字列が目立つようにするためです。

## どうやって？
PHPを使って文字列を大文字化する最も基本的な方法は`strtoupper()`関数を使用することです。以下に使用例とその出力を示します。

```PHP
<?php
$str = "こんにちは、世界！";
$result = strtoupper($str);
echo $result;
?>
```

このコードは以下の出力を生成します。

```PHP
"こんにちは、世界！";
```

## ディープダイブ
文字列大文字化の背景には特に歴史的な文脈はありませんが、極めて基本的な操作として、ほとんどのプログラミング言語にその機能が存在します。

また、大文字化の代わりに小文字化したい場合、PHPには`strtolower()`関数も存在します。具体的な使用例は以下の通りです：

```PHP
<?php
$str = "こんにちは、世界！";
$result = strtolower($str);
echo $result;
?>
```

PHPでは、大文字と小文字を無視した比較を行いたい場合、`strcasecmp()`関数が便利です。`strcasecmp()`関数は、大文字と小文字を区別せずに2つの文字列を比較します。

```PHP
<?php
$str1 = "こんにちは";
$str2 = "こんにちわ";
if(strcasecmp($str1, $str2) == 0) {
    echo "文字列は等しい";
} else {
    echo "文字列は等しくない";
}
?>
```

## 参照
* PHP公式ドキュメンテーションの`strtoupper()`関数：http://php.net/manual/ja/function.strtoupper.php
* PHP公式ドキュメンテーションの`strtolower()`関数：http://php.net/manual/ja/function.strtolower.php
* PHP公式ドキュメンテーションの`strcasecmp()`関数：http://php.net/manual/ja/function.strcasecmp.php