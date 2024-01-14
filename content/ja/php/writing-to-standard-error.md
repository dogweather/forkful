---
title:                "PHP: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ
PHPで標準エラーへの書き込みを行う理由は、デバッグやエラーの追跡に役立つからです。これにより、エラーがどこで発生したかを特定したり、プログラムを改善するための情報を収集したりすることができます。

## 方法
PHPでは、`error_log()`関数を使用して標準エラーへの書き込みを行うことができます。以下の例では、変数の値を標準エラーに書き込む方法を示します。

```PHP
<?php
$variable = "Hello World";

// 標準エラーに変数の値を書き込む
error_log("変数の値：" . $variable );
```

上記のコードを実行すると、標準エラーには以下のように出力されます。

`変数の値：Hello World`

## ディープダイブ
`error_log()`関数は、第2引数に`message_type`というオプションを指定することができます。このオプションを使用すると、エラーログの種類を指定することができます。デフォルトは`0`で、これはエラーメッセージをPHPのログに書き込みます。しかし、`3`を指定することでPHPのエラーログではなく標準エラーに書き込むことができます。以下の例を参考にしてください。

```PHP
<?php
$variable = "Hello World";

// 標準エラーに変数の値を書き込む
error_log("変数の値：" . $variable, 3 );
```

上記のコードを実行すると、標準エラーには以下のように出力されます。

`変数の値：Hello World`

## 参考資料
- [PHP公式ドキュメント - error_log()](https://www.php.net/manual/ja/function.error-log.php)
- [PHP公式ドキュメント - error_reporting()](https://www.php.net/manual/ja/function.error-reporting.php)
- [PHPマニュアル - エラー処理関数](https://www.php.net/manual/ja/ref.errorfunc.php)
- [Qiita - PHPでエラーログを標準エラーに書き込む方法](https://qiita.com/suin/items/639242253c882c6413c6)

## 関連リンク