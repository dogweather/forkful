---
title:                "Fish Shell: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜディレクトリが存在するかを確認する必要があるのか

ディレクトリが存在するかどうかを確認することは、プログラムを実行する前に重要なチェックです。例えば、ファイルを読み込む前にディレクトリが存在するかどうかを確認することで、エラーを防ぐことができます。

# ディレクトリの存在を確認する方法

ディレクトリが存在するかどうかを確認するには、```test -d```コマンドを使用します。このコマンドは、指定したパスがディレクトリであれば``1``を、そうでなければ``0``を返します。

```Fish Shell
if test -d /home/user/documents
  echo "The directory exists!"
else
  echo "The directory does not exist."
end
```

出力は以下のようになります。

```
The directory exists!
```

# 深堀りする

ディレクトリの存在を確認する際に、```test -d```コマンドではなく、```dirhish```コマンドを使用することもできます。これは、ディレクトリが存在しない場合にエラーを返す代わりに、空の値を返します。また、特定のディレクトリに移動する必要がある場合は、```cd```コマンドを使用することもできます。

```Fish Shell
if dirhish /home/user/documents
  cd /home/user/documents
  echo "Moved to the documents directory."
else
  echo "The directory does not exist."
end
```

出力は以下のようになります。

```
Moved to the documents directory.
```

## その他の参考リンク

- [Fish Shell公式サイト](https://fishshell.com/)
- [ディレクトリの存在を確認するコマンド一覧](https://fishshell.com/docs/current/commands.html#test-command)
- [「図解でわかるFish Shellの便利な使い方」](https://zine.longseller.org/fish-shell/)

＃参考リンク

- [Fish Shell Official Website]（https://fishshell.com/）
- [Directory existence confirmation command list]（https://fishshell.com/docs/current/commands.html#test-command）
- [“Easy-to-understand use of Fish Shell”]（https://zine.longseller.org/fish-shell/）