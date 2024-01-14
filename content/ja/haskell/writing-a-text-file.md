---
title:    "Haskell: 「テキストファイルの作成」"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## なぜ

プログラミング言語Haskellを使ってテキストファイルを書く理由は、コンピューターで文字を保存したり、データを処理したりするために必要です。

## 作り方

まず、テキストファイルを作成するために必要なライブラリをインポートします。
```Haskell
import System.IO
```

次に、ファイルを開き、内容を書き込みます。
```Haskell
main = do
  let fileName = "sample.txt"
  handle <- openFile fileName WriteMode
  hPutStrLn handle "こんにちは、世界！"
  hClose handle
```

最後に、このコードを実行すると、指定したファイルに文字列が書き込まれます。

## 深堀り

テキストファイルを作成する際には、ファイルを開いた後、必ず閉じる必要があります。これは、開いたファイルをクローズすることで、コンピューターのリソースを解放し、エラーが発生するリスクを減らすためです。

また、書き込む内容が複数行ある場合は、`hPutStrLn`ではなく`hPutStr`を使用して、改行を手動で挿入する必要があります。

## 関連リンク

- [Haskell公式サイト](https://www.haskell.org/)
- [Haskellのチュートリアル](http://learnyouahaskell.com/chapters)
- [Haskellの構文ガイド](https://wiki.haskell.org/Syntax_in_functions)
- [GitHubのHaskellリポジトリ](https://github.com/search?q=language%3AHaskell)
- [Haskellコミュニティーのフォーラム](https://discourse.haskell.org/)
- [Haskellでテキストファイルを読み書きする方法](https://qiita.com/PyYoshi/items/78798f66fd653575efc4)