---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "Bash: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックする理由は主に2つあります。１つは、スクリプトやプログラムを実行する前に、そのディレクトリが存在するか確認することで、実行時のエラーを防ぐことができるからです。もう１つは、特定のディレクトリが存在するかどうかを確認することで、条件分岐やループなどの制御フローを作成することができるからです。

## 使い方

```Bash
if [ -d /path/to/directory ]; then
    echo "The directory exists!"
else
    echo "The directory does not exist."
fi
```

上記のコードは、「/path/to/directory」というディレクトリが存在するかどうかをチェックし、存在すれば「The directory exists!」と出力し、存在しなければ「The directory does not exist.」と出力します。ここでは、「-d」オプションを使ってディレクトリが存在するかどうかを確認しています。

別の方法として、コマンドの終了ステータスを使ってディレクトリの存在をチェックする方法もあります。

```Bash
# コマンドの終了ステータスが「0」ならディレクトリは存在する
if grep -q "keyword" /path/to/directory/file; then
    echo "The directory exists!"
else
    echo "The directory does not exist."
fi
```

上記の例では、「grep」コマンドを使用して特定のキーワードが含まれるファイルが「/path/to/directory」ディレクトリ内に存在するかどうかをチェックしています。もしファイルが存在すれば「grep -q」コマンドの終了ステータスは「0」となり、if文の条件式が成り立ちます。

## 深堀り

Bashには、他にもディレクトリの存在をチェックする方法があります。例えば、「-e」オプションを使う方法もあります。「-e」オプションを使うと、ファイルだけでなくディレクトリの存在も確認することができます。

また、「-f」オプションを使うことで、存在するかどうかをチェックしたいファイルの種類を指定することもできます。「-f」オプションの後に以下のようなファイルタイプを指定することができます。

- 書き込み可能ファイル： file
- ディレクトリ： directory
- シンボリックリンク： link
- 等

例えば、「-f file」を指定すると、ファイルが存在し、かつ書き込み可能である場合に条件式が成り立ちます。

## 参考リンク

- [How to check if a directory exists in a Bash shell script](https://www.cyberciti.biz/faq/howto-check-if-a-directory-exists-in-a-bash-shellscript/)
- [How to check if a directory exists in Linux command line](https://linuxize.com/post/how-to-check-if-directory-exists-in-linux/#using-the-test-operator)
- [Bashにおけるディレクトリとファイルの存在をチェックする方法](https://dev.classmethod.jp/articles/bash-check-file-directory-exist/)

## 併せて参考になるリンク

- [Bash scriptの基本文法](https://qiita.com/yudoufu/items/7e1d8b02d74637d6cd19)
- [Bash scriptで簡単な条件分岐を書く方法](