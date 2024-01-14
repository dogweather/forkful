---
title:                "PHP: 「ディレクトリが存在するかどうかをチェックする」"
simple_title:         "「ディレクトリが存在するかどうかをチェックする」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリの存在をチェックすることの重要性は、プログラムで特定のファイルやディレクトリにアクセスする必要があるときに非常に重要です。存在しないディレクトリをチェックしないと、プログラムが失敗する可能性があります。ディレクトリの存在を確認することで、プログラムの安全性と信頼性が向上します。

## 使い方

ディレクトリが存在するかどうかを確認するには、`file_exists()`関数を使用します。以下のコード例を参考にし、ディレクトリが存在する場合には「ディレクトリが存在します」というメッセージを、存在しない場合には「ディレクトリが存在しません」というメッセージを表示します。

```PHP
if (file_exists("ディレクトリのパス")) {
    echo "ディレクトリが存在します";
} else {
    echo "ディレクトリが存在しません";
}
```
サンプル出力:
```
ディレクトリが存在します
```

## 深堀り

`file_exists()`関数は、あらゆる種類のファイルやディレクトリに対して使用することができます。また、ディレクトリが存在しない場合には`false`を返しますが、存在しないファイルやパーミッションの問題など、何かしらのエラーがあるときにも`false`を返すことがあります。そのため、`file_exists()`関数だけではディレクトリが存在するのかどうかの正確な判断ができません。ファイルやディレクトリに対するパーミッションなども含めて、より詳細な確認が必要です。

## 参考リンク

- [PHP: file_exists - Manual](https://www.php.net/manual/ja/function.file-exists.php)
- [How to Check if a Directory Exists in PHP - GeeksforGeeks](https://www.geeksforgeeks.org/how-to-check-if-a-directory-exists-in-php/)
- [PHP Directory Functions](https://www.php.net/manual/ja/ref.dir.php)