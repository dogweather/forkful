---
title:                "Fish Shell: ディレクトリの存在を確認する方法"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

```
Fish Shell```を使ってディレクトリの存在を確認するのにどうして携わるのか、と疑問に思う人もいるかもしれません。しかし、実際にはこの機能は非常に便利であり、コードの品質や可読性を向上させることにも役立ちます。ディレクトリの存在を確認することで、より安全なコードを書くことができるのです。

## 方法

まず、Fish Shellを開始して、下記のコマンドを入力します。

```
set -q 'dir'
```

これにより、現在のディレクトリが存在するかどうかが確認されます。もしディレクトリが存在する場合は、返り値として`1`が返されます。存在しない場合は、返り値として`0`が返されます。

次に、より複雑な例を見てみましょう。下記のようなコードを入力します。

```
if set -q 'dir'
  echo "ディレクトリが存在します"
else
  echo "ディレクトリが存在しません"
end
```

ここでは、`if`文を使ってディレクトリの存在を確認し、存在する場合はメッセージを表示し、存在しない場合は別のメッセージを表示するようにしています。

## 深堀り

Fish Shellでは、`test`コマンドを使ってディレクトリの存在を確認することもできます。例えば、下記のようなコマンドを入力すると、ディレクトリの存在を確認し、存在する場合は`true`を、存在しない場合は`false` を返します。

```
test -d 'dir'
```

また、`and`や`or`を使うことで、複数のディレクトリの存在を同時に確認することもできます。例えば、下記のようなコードを入力すると、複数のディレクトリが全て存在する場合のみ、メッセージを表示するようになります。

```
test -d 'dir1' and test -d 'dir2' and test -d 'dir3'
echo "全てのディレクトリが存在します"
```

## 参考リンク

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [How to Check if a Directory Exists in Fish Shell](https://linuxize.com/post/how-to-check-if-a-directory-exists-in-fish-shell/)
- [Understanding Fish Shell Conditionals](https://fishshell.com/docs/current/index.html#conditionals)