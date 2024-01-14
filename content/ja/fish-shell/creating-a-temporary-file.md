---
title:    "Fish Shell: 一時的なファイルの作成"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

##なぜ

プログラムを作成する際に、一時的なファイルを作成する理由は様々あります。例えば、一時的にデータを保存するためや、特定の処理を行うためにファイルを作成することが必要な場合があります。

##作り方

一時的なファイルを作成する方法はさまざまありますが、ここではFish Shellを使用した方法を紹介します。```Fish Shell```コードブロック内にコーディング例とサンプルの出力を記載します。

```
# 一時的なファイルの作成方法（例）
echo "一時的なファイルを作成しています..."
set temp_file (mktemp)  # ファイル名は自動で生成されます
echo "これは一時的なファイルです" > $temp_file # ファイルにデータを書き込みます
ls $temp_file  # ファイルをリスト表示します
```

上記のコードを実行すると、一時的なファイルが生成され、そのファイル名が表示されます。また、ファイルには指定したデータが書き込まれていることも確認できます。

##ディープダイブ

一時的なファイルを作成する際に使用される```mktemp```コマンドは、実際にはUnixコマンド「```tempfile```」に基づいています。このコマンドはランダムなファイル名を生成し、そのファイルを作成するための一時的な場所を指定します。ただし、このファイルは一時的なものであり、プログラムが終了すると自動的に削除されます。

##関連リンク

- [Fish Shell公式サイト](https://fishshell.com/)
- [Fish Shell勉強会イベント情報](https://fishshell.connpass.com/)
- [Unixコマンド「tempfile」についての詳細情報](https://www.mkssoftware.com/docs/man3/tempfile.3.asp)

##参考文献

- [Temporary File in Shell Scripting](https://www.linuxjournal.com/content/temporary-files-shell-scripting) by Mitch Frazier, Linux Journal
- [Fish Shell Cookbook](https://github.com/jorgebucaran/fish-shell-cookbook) by Jorge Bucaran
- [Unixの基礎知識](http://www.nsl.tuis.ac.jp/xoops/modules/pico21/index.php?content_id=54) 著者：森田裕樹