---
title:                "Haskell: 「標準エラーへの書き込み」"
simple_title:         "「標準エラーへの書き込み」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

コンピュータープログラミングをするために、標準エラーに書き込むことが必要な場合があります。たとえば、バグが発生した際にエラーメッセージを表示するために使われることがあります。

## 方法

```Haskell
printToStdErr :: IO ()
printToStdErr = do
    hPutStrLn stderr "エラーメッセージ"
    hFlush stderr
```

このように、Haskellでは `stderr` を使って標準エラーに書き込むことができます。また、上記のコードでは `hFlush` を使ってエラーメッセージを即座に出力するようにしています。

## 深堀り

標準エラーに書き込むことで、プログラムの実行中に起こるエラーをユーザーに伝えることができます。`stderr` は常にエラーメッセージを表示するために使われるわけではありませんが、必要となる場面で役立つ重要な機能です。

## 参考リンク

- [Haskell入門](https://qiita.com/nosix/items/101378cc2bd1539aec3c)
- [標準入出力とファイル入出力](http://mmatsubara.hatenablog.com/entry/2016/03/17/111921)
- [Haskellでデバッグする方法](http://ai-ya-no.diary.jp/posts/2015/06/12/haskell%E3%81%A7%E3%83%87%E3%83%90%E3%83%83%E3%82%B0%E3%81%99%E3%82%8B%E6%96%B9%E6%B3%95/)