---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

一時ファイルの作成は、一時的にデータを保存する手法です。プログラマーがこうする理由は、大きな操作を行うときに、元のデータが壊れるのを防ぐためだったり、コンピュータがクラッシュした場合に、元のデータが失われるのを防ぐためです。

## やり方：

Fish Shell で一時ファイルを作成するには以下のようにコマンドを打ちます：

```Fish Shell
set tmpfile (mktemp)
echo "一時的なデータ" > $tmpfile
```

これにより、新たな一時ファイルが作成され、そのファイルに"一時的なデータ"というテキストが保存されます。

## 深掘り：

一時ファイルの概念は初期のコンピューターシステムから存在しました。旧式のOSでは、一時ファイルを明示的に削除しなければならず、それがしばしば忘れられ、ディスクスペースの無駄になっていました。近代的なシステムでは、OSが開始時や定期的に一時ファイルを自動的に削除します。

`mktemp`コマンド以外にも一時ファイルを作成する方法はありますが、一時ファイル名の衝突を避けるためにファイルを作成すると同時にその名前を生成するため、`mktemp`が一般的に推奨されます。

また、Fish Shellでは`mktemp`が標準的に使われていますが、他のシェルでは`tempfile`など他のコマンドが使われることもあります。

## 関連するもの：

- Fish Shellのマニュアル：https://fishshell.com/docs/current/index.html
- `mktemp`のマニュアル：https://www.man7.org/linux/man-pages/man1/mktemp.1.html