---
title:                "標準エラーへの書き込み"
html_title:           "Haskell: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

なぜ標準エラー出力を行うのか、短く説明します。Haskellのプログラムで発生したエラーを追跡し、バグを見つける時には、標準エラー出力が役に立ちます。 そうすることで、より効率的に問題を解決することができます。

## How To

```Haskell
import System.IO (hPutStrLn, stderr)

-- 標準エラー出力へのメッセージの書き込み
hPutStrLn stderr "エラーメッセージを書き込みます。"

-- 数値のエラーを示すために、 "error" を使用することもできます
hPutStrLn stderr $ "エラーが発生しました。数値: " ++ show 500

-- リストのエラーを示すために、 "error" と "unwords" を使用することもできます
hPutStrLn stderr $ error $ "エラーが発生しました。リスト: " ++ unwords ["A", "B", "C"]
```

上記のように、Haskellでは、`System.IO`モジュールの`hPutStrLn`関数を使用して、標準エラー出力へのメッセージの書き込みができます。一般的なエラーメッセージを手動で書き込むこともできますが、`error`関数を使用することで、より明確なエラーメッセージを生成することができます。また、文字列操作に便利な`show`関数や`unwords`関数を組み合わせて使用することで、さまざまな種類のエラーを示すメッセージを生成することができます。

## Deep Dive

上記の例では、`error`関数を使用してエラーメッセージを生成しましたが、実際には、`Control.Monad`モジュールの`MonadFail`クラスのインスタンスである`fail`関数を使用することでさらに柔軟なエラーハンドリングができます。また、標準エラー出力を使用するだけでなく、`System.IO`モジュールの他の関数を使用することで、エラーメッセージをファイルやターミナル以外の場所にも書き込むことができます。

## See Also

- [System.IOモジュールのドキュメント](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Control.Monadモジュールのドキュメント](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html)
- [Haskellのエラーハンドリングの詳細](https://en.wikibooks.org/wiki/Haskell/Error_handling)