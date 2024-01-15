---
title:                "一時ファイルの作成"
html_title:           "Bash: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ？

一時ファイルを作成する理由はさまざまですが、一般的な使い方は一時的にデータを保存することです。データの処理やバックアップなど、一時的な作業には一時ファイルが非常に便利です。

## 作り方

Bashで一時ファイルを作成するには、`mktemp`コマンドを使用します。

```bash
tempfile=$(mktemp)
echo "Hello World" > $tempfile
```

これにより、一時ファイルが作成され、そのパスが`$tempfile`変数に格納されます。次に、ファイルに書き込むことができます。

上の例では、`Hello World`というテキストが一時ファイルに書き込まれます。一時ファイルを使用した後は、`rm`コマンドでファイルを削除することができます。

```bash
rm $tempfile
```

一時ファイルはデフォルトで`/tmp`ディレクトリに作成されますが、`mktemp`コマンドにオプションを追加することで、作成するディレクトリやファイル名のプレフィックスを指定することもできます。

```bash
mktemp -d ~/Downloads/tempXXX
```

上記の例では、デスクトップの`Downloads`フォルダに`temp`というプレフィックスを持つ一時ディレクトリが作成されます。

## 深堀り

一時ファイルを作成する際に、注目すべきポイントがいくつかあります。まず、`mktemp`コマンドは、一時ファイルを安全に作成するためにランダムな文字列を使用します。これにより、ファイル名が重複することを防ぎます。

また、一時ファイルは通常、プログラムが終了すると自動的に削除されます。しかし、場合によっては手動で削除する必要があります。

さらに、一時ファイル作成時には必ず、ファイルのパーミッションを厳密に設定することが重要です。通常、一時ファイルには不要なアクセスを許可しないようにするため、読み書きの権限を制限します。

## 関連情報

- Unixコマンド: [mktempコマンドのマニュアル](https://man7.org/linux/man-pages/man1/mktemp.1.html)
- Bashスクリプト: [Bashの一時ファイルの作り方と削除方法](https://www.shellhacks.com/ja/create-temporary-files-in-bash-script/)
- Linuxチュートリアル: [一時ファイルを作成する方法](https://www.linode.com/docs/tools-reference/tools/create-temporary-files-in-bash/)