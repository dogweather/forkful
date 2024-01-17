---
title:                "テキストファイルの書き方"
html_title:           "PowerShell: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 何＆なぜ？

テキストファイルを書くとは、パソコン上にある情報を文字や数字の形で保存することを指します。プログラマーは主に、それぞれのプログラムやコードの結果やログを保存するためにテキストファイルを書くことがあります。

## 方法

```PowerShell
# テキストファイルを作成する
"こんにちは！これはテストファイルです。" | Out-File test.txt

# 存在するテキストファイルに追記する
"追記する文章" | Out-File -Append test.txt

# ファイルから情報を読み込む
Get-Content test.txt
```

上記のコードを実行すると、まずはじめに"test.txt"という名前のファイルが作成されます。その後、ファイルに"こんにちは！これはテストファイルです。"という文章が書き込まれ、さらに"追記する文章"が追加されます。最後に、書き込まれた情報が読み込まれてコンソールに表示されます。

## より詳しく

テキストファイルの歴史は古く、プログラミング言語の初期から使用されてきました。テキストファイルの代わりに、データベースなどのより高度なデータベースを使用することもできますが、簡単に情報を保存することができるテキストファイルは今でも重要な役割を果たしています。また、PowerShell以外にも、BashやCMDなどのシェルスクリプトでもテキストファイルを書くことができます。

## 関連リンク

- [PowerShellドキュメント](https://docs.microsoft.com/ja-jp/powershell)
- [Bashドキュメント](https://www.gnu.org/software/bash/manual/)
- [CMDドキュメント](https://docs.microsoft.com/ja-jp/windows-server/administration/windows-commands/windows-commands)