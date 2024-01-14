---
title:    "Haskell: 「標準エラーに書き込む」"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# なぜ「標準エラー出力」に書き込むのか

プログラミングをする際、コードの実行中にエラーが発生することはよくあります。エラーメッセージはソフトウェアをデバッグする際に大変役立つものです。しかし、そのエラーメッセージがコンソール上に表示されてしまうと、プログラマーはメインの出力と混ざってわかりにくくなってしまいます。そこで、別の場所にエラーメッセージを表示する必要があります。その場所が「標準エラー出力」です。

# やり方

標準エラー出力に書き込む方法は簡単です。以下のように、```Haskell```コードブロックに```System.IO```をインポートして、```stderr```関数を使って書き込めば良いです。

```Haskell
import System.IO

main = do
  hPutStrLn stderr "このメッセージは標準エラー出力に書き込まれます。"
```

上記のコードを実行すると、コンソール上には何も出力されませんが、標準エラー出力にはメッセージが表示されます。

# ディープダイブ

標準エラー出力は、通常の出力とは異なるチャンネルを通じてメッセージを送信するため、重要なデバッグツールとして使われます。また、リダイレクションやパイプなどの機能も使うことができます。例えば、以下のようなコマンドを実行すると、標準エラー出力をファイルにリダイレクトすることができます。

```
$ runhaskell Main.hs > main.txt 2> error.txt
```

上記のコマンドを実行すると、```main.txt```ファイルにはメインの出力が、```error.txt```ファイルにはエラーメッセージが出力されます。

# 関連情報

- [Haskellドキュメント - System.IO](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html#v:stderr)
- [Haskell Wikibook - ファイルへの出力](https://en.wikibooks.org/wiki/Haskell/Understanding_IO/Writing_files)