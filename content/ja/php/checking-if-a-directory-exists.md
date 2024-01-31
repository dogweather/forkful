---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:57:42.723071-07:00
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"

category:             "PHP"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

ディレクトリが存在するかどうかのチェックとは、ファイルシステム上に特定のフォルダ（ディレクトリ）が存在するかを確認する行為です。プログラマーはファイル操作やデータ保存前に誤った操作を避け、エラーを防ぐためにこれを行います。

## How to: (方法：)

PHPでは`is_dir`関数を使ってディレクトリの存在を簡単にチェックできます。以下のコードを見てみましょう。

```php
<?php
$directoryPath = "/path/to/your/directory";

if (is_dir($directoryPath)) {
    echo "ディレクトリが存在します。";
} else {
    echo "ディレクトリが見つかりません。";
}
?>
```

実行すると、ディレクトリが存在する場合は「ディレクトリが存在します。」と出力され、存在しない場合は「ディレクトリが見つかりません。」と出力されます。

## Deep Dive (掘り下げ：

`is_dir`関数はPHP 4以降で使用できます。これはシステムレベルの呼び出しをするため、処理は速いです。ただし、実行する環境のファイルシステムの違いに注意が必要です。

代替手段として`file_exists`関数がありますが、これはファイルとディレクトリの両方の存在をチェックします。そのため、厳密にディレクトリだけを確認したい場合は`is_dir`をお勧めします。

実装の詳細においては、有効なパスかどうかも重要です。相対パスの解釈は実行時の現在のディレクトリに依存するため、スクリプトが想定外の場所で実行されると問題が発生する可能性があります。

## See Also (関連情報):

- PHP公式ドキュメント `is_dir`: https://www.php.net/manual/ja/function.is-dir.php
- PHP公式ドキュメント `file_exists`: https://www.php.net/manual/ja/function.file-exists.php
