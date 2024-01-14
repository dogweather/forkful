---
title:    "PHP: 一時ファイルの作成"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜプログラムで一時ファイルを作成するのか？

一時ファイルを作成することは、プログラムの実行中に一時的にデータを保存するための非常に便利な方法です。データを一時ファイルに保存することで、本当のファイルに書き込む前にプログラムをテストすることができます。また、一時ファイルを使用することで、不必要なデータの保持を防ぎ、プログラムのパフォーマンスを改善することができます。

## 作り方

```PHP
// ランダムな一時ファイル名を生成
$temp_filename = tempnam(sys_get_temp_dir(), "temp");

// ファイルにデータを書き込む
$data = "これは一時ファイルに書き込んだデータです。";
file_put_contents($temp_filename, $data);

// ファイルからデータを読み取り
echo file_get_contents($temp_filename);

// ファイルの削除
unlink($temp_filename);
```

### 出力結果：

```
これは一時ファイルに書き込んだデータです。
```

## ディープダイブ

プログラムで一時ファイルを作成する方法は様々ありますが、一番よく使用される方法は`tempnam()`関数を使用することです。この関数は、指定されたディレクトリにランダムなファイル名で一時ファイルを作成することができます。また、`sys_get_temp_dir()`という関数を使用することで、システムの一時ディレクトリを取得することができます。

しかし、プログラムが終了すると作成した一時ファイルは自動的に削除されるため、一時ファイルを保持する場合には`unlink()`関数を使用して明示的に削除する必要があります。

## See Also

- [PHP: `tempnam()`関数](https://www.php.net/manual/ja/function.tempnam.php)
- [PHP: `sys_get_temp_dir()`関数](https://www.php.net/manual/ja/function.sys-get-temp-dir.php)
- [PHP: `file_put_contents()`関数](https://www.php.net/manual/ja/function.file-put-contents.php)
- [PHP: `file_get_contents()`関数](https://www.php.net/manual/ja/function.file-get-contents.php)
- [PHP: `unlink()`関数](https://www.php.net/manual/ja/function.unlink.php)