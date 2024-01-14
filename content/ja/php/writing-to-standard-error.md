---
title:    "PHP: 「標準エラーへの書き込み」"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# なぜstandard errorに書き込むのか

PHPプログラミングを行う上で、たびたび必要となるのがstandard errorへの書き込みです。標準エラーは、エラーメッセージやデバッグ情報を出力するためのチャネルであり、プログラマーにとって非常に重要です。そこで今回は、なぜstandard errorへの書き込みが必要なのかをご紹介します。

## 書き込みの方法

PHPでstandard errorへの書き込みを行うには、主に二つの方法があります。一つは、```error_log()```関数を使用する方法です。この関数は、指定したメッセージをエラーログに書き込むことができます。使用方法は以下の通りです。

```PHP
<?php
    error_log("エラーメッセージ");
?>
```
このようにすることで、指定したメッセージがエラーログに書き込まれます。

もう一つの方法は、PHPの組み込み定数である```STDERR```を使用する方法です。この方法では、```fwrite()```関数を使用して直接standard errorに書き込むことができます。具体的なコード例は以下の通りです。

```PHP
<?php
    $file = fopen('php://stderr', 'w');
    fwrite($file, "エラーメッセージ");
    fclose($file);
?>
```

## 深堀り

standard errorへの書き込みを行う際には、いくつか注意点があります。まず、```error_log()```関数を使用する場合には、第二引数にログの出力先を指定することができます。ここで指定できる値は、「ファイルパス」、「0」または「1」のいずれかです。ファイルパスを指定した場合は、指定したファイルにログが書き込まれます。0を指定した場合は、エラーログがphp.iniで指定されたデフォルトの場所に書き込まれます。1を指定した場合は、エラーログがサーバーのエラーログに書き込まれます。

また、```fwrite()```関数を使用する際には、```fopen()```関数の第二引数に```'w'```を指定することで、ファイルをオープンすることができます。この時、既に存在するファイルをオープンすると、そのファイルの内容がクリアされます。そのため、追記したい場合は```'a'```を指定することでファイルの最後に追記されるようにすることができます。

# ご参考

- [PHP: error_log - Manual](https://www.php.net/manual/ja/function.error-log.php)
- [PHP: fwrite - Manual](https://www.php.net/manual/en/function.fwrite.php)
- [PHP: fopen - Manual](https://www.php.net/manual/en/function.fopen.php)