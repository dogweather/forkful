---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

### 何となぜ?
コマンドライン引数は、コマンドラインからアプリケーションへの入力です。これを用いることでプログラムはより柔軟で再利用可能になります。

### 使用方法:
次のコードはコマンドライン引数を読み取るHaskellプログラムの基本形です。

```haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    print args
```
これを実行すると、コマンドライン引数をリスト形式で表示します。例えば、`runhaskell Test.hs arg1 arg2 arg3` のように実行すると、 `["arg1","arg2","arg3"]` の形式で出力されます。

### ディープダイブ:
コマンドライン引数は Unixシステム読み出しの歴史を持つ古老の仕組みです。その代替として環境変数や設定ファイルがありますが、状況によりコマンドライン引数が最適な場合もあります。Haskellの `getArgs` 関数は最も基本的な方法で、これを使用すると全ての引数をStringのリストとして取得します。より高度な操作が必要な場合はライブラリを検討してみてください。

### 参照文献:
- [Real World Haskell: コマンドライン引数](http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html)
- [Haskell Wiki: コマンドライン引数](https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling) 
- [Hackage: optparse-applicative パッケージ](https://hackage.haskell.org/package/optparse-applicative)