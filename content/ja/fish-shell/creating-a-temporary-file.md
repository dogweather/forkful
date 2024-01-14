---
title:    "Fish Shell: 一時ファイルの作成"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成することの利点は、一時的にデータを保存することができるため、処理の途中でデータを保持したり、データを一時的に変更したりすることができます。また、プログラムの実行中にエラーが発生した場合にも、作成した一時ファイルを確認することでエラーの原因を特定することができます。

## 作り方

```Fish Shell
# 一時ファイルを作成する
set tmp_file (mktemp)

# 作成した一時ファイルにデータを書き込む
printf "Hello, world!" > $tmp_file

# データを読み込んで出力する
cat $tmp_file 
```

出力結果:

```
Hello, world!
```

## 深堀り

一時ファイルを作成する際、Fish Shellでは`mktemp`コマンドを使用します。このコマンドは一時ファイルを作成し、そのファイル名を出力します。一時ファイルは`/tmp`ディレクトリに作成されますが、オプションを使用することで作成するディレクトリを指定することもできます。

また、作成した一時ファイルは必要な際に手動で削除する必要があります。Fish Shellでは、`rm`コマンドで一時ファイルを削除することができます。

## 関連リンク

- [Fish Shell公式ウェブサイト](https://fishshell.com/)
- [Fish Shellドキュメンテーション](https://fishshell.com/docs/current/index.html)
- [一時ファイルの作成方法について](https://qiita.com/onokatio/items/90a4526f84509ee5f105)