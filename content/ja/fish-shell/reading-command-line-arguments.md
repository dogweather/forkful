---
title:                "Fish Shell: コンピュータープログラミングの記事のタイトル：「コマンドライン引数の読み取り」"
simple_title:         "コンピュータープログラミングの記事のタイトル：「コマンドライン引数の読み取り」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ
コマンドライン引数を読み込むことの重要性について説明します。

コマンドライン引数を読み込むことにより、プログラムを実行する際に指定したパラメーターを受け取ることができます。これにより、実行時に柔軟に動作を変更することができるようになります。

## 使い方
コマンドライン引数を読み込むためには、Fish Shellの組み込み機能である"$argv"を使用します。以下のコードブロックを参考にしてください。

```Fish Shell
# 例: "hello.sh"というファイルを実行する際に引数として"World"を渡した場合
#!/bin/fish

echo "Hello $argv[1]" # output: Hello World
```

このように、$argvを使用することで、実行時にどの引数が渡されたかを配列として取得することができます。

## 深堀り
コマンドライン引数を扱う際には、注意すべき点がいくつかあります。

まず、引数が渡されなかった場合、$argvの長さは1となります。これにより、引数が渡されなかったかどうかを簡単にチェックすることができます。

また、$argvには最初の要素として実行ファイル名が含まれるため、通常の引数は2番目以降から利用できます。

さらに、引数に空白やシェル特殊文字が含まれる場合、特殊文字をエスケープする必要があります。例えば、```echo "Hello $argv[1]"```というコマンドを実行する際に、引数として"Hello, World!"を渡したい場合は、"Hello, World!"という文字列をダブルクォーテーションで囲まなければなりません。

## その他の参考リンク
- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/cmds/set.html)
- [CodecademyのFish Shellチュートリアル](https://www.codecademy.com/learn/learn-the-command-line/modules/fish-shell)
- [コマンドライン引数の扱い方 - Qiita](https://qiita.com/pochman/items/a10ee666b6ddde1489b9)

## さらに参考になる情報
- [コマンドライン引数を取得する方法まとめ - postd](https://postd.cc/how-to-read-command-line-arguments-in-ruby/)
- [シェルスクリプトでのコマンドライン引数の利用方法 - Qiita](https://qiita.com/mikakane/items/b4325435b4730fa0b98c)
- [コマンドライン引数を処理するプログラムの作り方 - Think IT](https://thinkit.co.jp/free/article/0712/3/1/)