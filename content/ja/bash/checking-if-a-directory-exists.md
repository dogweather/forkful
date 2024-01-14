---
title:                "Bash: ディレクトリの存在をチェックする"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜディレクトリが存在するかをチェックするか

Bashプログラミングを行っているときに、特定のディレクトリが存在するかどうかを確認する必要があることがあります。これは、ファイル操作やスクリプトの実行において、プログラマーがコードを制御するために重要なステップです。ディレクトリが存在するかどうかを確認することで、スクリプトの実行を安全かつ成功させることができます。

## 利用方法

ディレクトリが存在するかどうかを確認するには、コマンドラインで以下のコードを入力します。

```Bash
#!/bin/bash
if [ -d /path/to/directory ]
then
  echo "ディレクトリが存在します"
else
  echo "ディレクトリが存在しません"
fi
```

このコードでは、`-d`のオプションを使用し、`/path/to/directory`におけるディレクトリの存在を確認しています。もしディレクトリがあれば、`echo`コマンドでディレクトリが存在することを表示し、存在しない場合は存在しないことを表示します。

また、もし存在しないディレクトリの作成を試みる場合は、以下のようにコードを書くこともできます。

```Bash
#!/bin/bash
if [ ! -d /path/to/directory ]
then
  mkdir /path/to/directory
fi
```

このコードでは、`!`を使用してディレクトリが存在しない場合にのみディレクトリを作成しています。

## 深堀り

ディレクトリが存在するかどうかを確認する方法は他にもあります。確認するには、`test`コマンドや`[`コマンドを使用することもできます。

また、ディレクトリが存在しない場合は特定のコマンドを実行することもできます。例えば、`/path/to/directory`が存在しない場合は`echo`コマンドを実行するときに、`&&`を使用して次のようにコードを書くことができます。

```Bash
#!/bin/bash
[ ! -d /path/to/directory ] && echo "ディレクトリが存在しません"
```

また、`directory`というディレクトリが存在するかどうかを確認したい場合は、`directory`という名前のファイルが存在するかどうかを確認することで可能です。

```Bash
#!/bin/bash
if [ -e directory ]
then
  echo "ディレクトリが存在します"
fi
```

## 他の参考ページ

- [Bashで条件式を記述する](https://script-archive.com/memo/HowtoBash/scripts_conditional_expression/)
- [ディレクトリが存在するかどうかをチェックするコマンド](https://qiita.com/yudoufu/items/fac3de8b549593a4758a)