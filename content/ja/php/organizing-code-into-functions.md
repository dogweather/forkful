---
title:                "コードを関数に整理する"
aliases:
- ja/php/organizing-code-into-functions.md
date:                  2024-01-26T01:11:54.593365-07:00
model:                 gpt-4-1106-preview
simple_title:         "コードを関数に整理する"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## なぜ関数にコードを整理するのか？
コードを関数に整理することは、定義された目的を持つ再利用可能なブロックにコードを分割することについてです。私たちは、物事を整頓し、冗長性を防ぎ、デバッグを容易にするためにこれを行います。

## 方法：
ユーザーに挨拶するための繰り返しコードがあると想像してください。代わりに、`greet_user` のような関数でそれをラップしましょう：

```php
function greet_user($name) {
    return "Hello, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

出力：
```
Hello, Alice!
Hello, Bob!
```

これで、挨拶したいときに毎回同じコード行を書き直さずに、いつでも使える便利なツールが手に入りました。

## 深掘り
関数はFORTRANの50年代初期からプログラミングにありました。それらは構造化プログラミングの基石であり、モジュール性と分離についてのものです。代替案ですか？クラスやメソッドについて話すオブジェクト指向に進むことができます。それらは、ファンシーなスーツを着た関数です。PHPについては、実装の詳細には、パラメータのデフォルト値の指定、入力の型ヒント、配列を使用するかPHP 7.1以降ではリストを使用して複数の値を返す機能が含まれます。

ここでは型宣言とデフォルト値が使われた現代的なひねりを見てみましょう：

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4は、配列操作で一般的に使用される、簡潔なワンライナー関数を書くのに役立つアロー関数も導入しました：

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

出力：
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## 関連情報
- [関数についてのPHPマニュアル](https://www.php.net/manual/ja/functions.user-defined.php)
- [PHP: 正しい方法 - 関数](https://phptherightway.com/#functions)
- [PHP 7.4アロー関数について学ぶ](https://stitcher.io/blog/short-closures-in-php)
