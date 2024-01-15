---
title:                "一時ファイルの作成"
html_title:           "PHP: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why
なぜ一時ファイルを作成するのか？この記事では、一時ファイルの作成がどのような状況で必要になるかを説明し、その利点について紹介します。

## How To
一時ファイルを作成するには、まず 「tempnam()」関数を使用してファイル名を作成します。次に、そのファイル名を使用して「fopen()」関数を使用してファイルを作成します。最後に、必要な処理が完了したらファイルを削除します。

```PHP
$tempFile = tempnam(sys_get_temp_dir(), 'prefix_'); //一時ファイル名を作成
$file = fopen($tempFile, 'w+'); //ファイルを作成
fwrite($file, "Sample text"); //ファイルにテキストを書き込む
fclose($file); //ファイルを閉じる
unlink($tempFile); //ファイルを削除
```

一時ファイルを使用する最も一般的な例は、ファイルアップロードの処理です。アップロードされたファイルは一時ファイルとして作成され、必要な処理が完了した後に削除されます。また、一時ファイルを使用することで、セキュリティ上の懸念を軽減することもできます。

## Deep Dive
一時ファイルは、一時的にデータを保存するために使用される一時的なファイルです。一時ファイルは、一時的な情報を保存することができるため、メモリやディスクスペースを節約することができます。

また、一時ファイルはサーバーのクラッシュなどの予期せぬ問題が発生した場合にも有用です。アップロードされたファイルなどが、一時ファイルとして作成されていれば、クラッシュした後でも再度処理を行うことができます。

しかし、一時ファイルを使用する際には注意が必要です。セキュリティ上の理由から、一時ファイルはアクセス可能な場所に保存されている必要があります。

## See Also
* [tempnam() 関数](https://www.php.net/manual/ja/function.tempnam.php)
* [fopen() 関数](https://www.php.net/manual/ja/function.fopen.php)
* [unlink() 関数](https://www.php.net/manual/ja/function.unlink.php)