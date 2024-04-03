---
date: 2024-01-20 17:53:10.739589-07:00
description: "How to: (\u65B9\u6CD5) PHP\u3067\u306F\u3001`echo`\u3084`print_r()`\u3001\
  `var_dump()`\u3092\u4F7F\u3063\u3066\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u304C\u3067\
  \u304D\u307E\u3059\u3002\u3053\u3053\u306B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u793A\
  \u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.250187-06:00'
model: gpt-4-1106-preview
summary: "PHP\u3067\u306F\u3001`echo`\u3084`print_r()`\u3001`var_dump()`\u3092\u4F7F\
  \u3063\u3066\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u304C\u3067\u304D\u307E\u3059\u3002\
  \u3053\u3053\u306B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059."
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

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
