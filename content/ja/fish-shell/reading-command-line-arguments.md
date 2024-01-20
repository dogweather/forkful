---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？
コマンドライン引数を読むとは、ユーザーがプログラムの実行時に追加入力を提供する方法です。プログラマーがこれを行う理由は、プログラムの柔軟性と動的機能を向上させるためです。

## どうやって？:

ここにFish Shellのコード例と出力結果があります。

```Fish Shell
# 実行するためのスクリプト例（example1.fish）
echo "引数の数: " (count $argv)
for arg in $argv
  echo "引数: " $arg
end

# 後で以下のようにスクリプトを実行します
> fish example1.fish 1 2 3 4
```

出力結果：

```Fish Shell
引数の数: 4
引数: 1
引数: 2
引数: 3
引数: 4
```

## ディープダイブ:
1.歴史的文脈: コマンドライン引数は古代のユニックス・デイズから存在し、当時のシェルスクリプトがより動的に操作をするために必要でした。

2.代替手段: 他にも、read組み込みコマンドを使用して直接ユーザー入力を読み込む方法もあります。

3.実装詳細: Fish Shellでは、$argv変数を用いてコマンドライン引数にアクセスします。これらの引数は文字列のリストとして格納され、個別に引用符で引かれた文字列として扱われます。

## 参照先:
1. Fish Shell公式ドキュメンタイション: https://fishshell.com/docs/current/index.html
2. コマンドライン引数に関する良質なホームページ: https://www.learnshell.org/
3. ユーザー入力の読み取りに関する詳細なガイド: https://fishshell.com/docs/current/cmds/read.html