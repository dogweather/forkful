---
title:                "ディレクトリの存在を確認する"
html_title:           "Fish Shell: ディレクトリの存在を確認する"
simple_title:         "ディレクトリの存在を確認する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認する方法を知ることで、コーディングの効率を上げることができます。

## 使い方

```Fish Shell```を使用すると簡単にディレクトリの存在を確認することができます。例えば、```test```という名前のディレクトリが存在するかどうかを確認するには、次のように入力します。

```fish
if test -d test 
    echo "ディレクトリが存在します"
else 
    echo "ディレクトリは存在しません"
end
```

これにより、ディレクトリが存在する場合は「ディレクトリが存在します」というメッセージが表示され、存在しない場合は「ディレクトリは存在しません」というメッセージが表示されます。

## ディープダイブ

ディレクトリの存在を確認するために使用するコマンドは```test -d```です。このコマンドはファイルやディレクトリが存在するかどうかを確認することができます。```-d```はディレクトリの存在を確認するためのオプションです。```test```コマンドはUnixシステムでよく使用されるコマンドであり、Fish Shellでも使用することができます。

## 関連情報

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Unixのtestコマンドについてのドキュメント](https://linuxjm.osdn.jp/html/GNU_gettext/glibc/libc_330.html)