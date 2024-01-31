---
title:                "デバッグ出力を表示する"
date:                  2024-01-20T17:53:10.739589-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
デバッグ出力とは、コードの動作をチェックして問題を見つけるために使うメッセージです。これにより、プログラマーはコードがどのように動いているかを理解し、エラーを素早く修正できます。

## How to: (方法)
PHPでは、`echo`や`print_r()`、`var_dump()`を使ってデバッグ出力ができます。ここに基本的な例を示します。

```PHP
<?php
$variable = array('apple', 'orange', 'banana');

// Basic output
echo 'I have ' . count($variable) . ' fruits.';

// Print human-readable information about a variable
print_r($variable);

// Output with more details
var_dump($variable);
?>
```

出力例：
```
I have 3 fruits.
Array (
    [0] => apple
    [1] => orange
    [2] => banana
)
array(3) {
  [0]=>
  string(5) "apple"
  [1]=>
  string(6) "orange"
  [2]=>
  string(6) "banana"
}
```

## Deep Dive (深みへ)
歴史としては、`print_r()`と`var_dump()`がPHP 4に導入され、コンソールベースのデバッグに利用されてきました。アルタナティブとしては、Xdebugやログファイルに書き出す方法があります。デバッグ出力の実装詳細では、`var_dump()`はデータ型も含めた詳細な情報を出力し、`print_r()`は読みやすい情報を提供しますが、データ型は含まれません。`echo`は単純な文字列出力に使われます。

## See Also (関連情報)
- [PHP: echo - Manual](https://www.php.net/manual/en/function.echo.php)
- [PHP: print_r - Manual](https://www.php.net/manual/en/function.print-r.php)
- [PHP: var_dump - Manual](https://www.php.net/manual/en/function.var-dump.php)
- [Xdebug - Debugger and Profiler Tool for PHP](https://xdebug.org/)
