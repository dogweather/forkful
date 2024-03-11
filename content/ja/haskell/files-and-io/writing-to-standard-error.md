---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:32.541998-07:00
description: "Haskell\u3067\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u306B\u66F8\
  \u304D\u8FBC\u3080\u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u901A\
  \u5E38\u306E\u7D50\u679C\u3068\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\
  \u533A\u5225\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u3053\u3068\u3092\u53EF\
  \u80FD\u306B\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u554F\u984C\u306E\u30B7\
  \u30B0\u30CA\u30EA\u30F3\u30B0\u3068\u30C7\u30D0\u30C3\u30B0\u306B\u4E0D\u53EF\u6B20\
  \u3067\u3042\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u4E3B\u8981\u306A\u30C7\
  \u30FC\u30BF\u3084\u7D50\u679C\u3092\u3057\u3070\u3057\u3070\u6301\u3064\u6A19\u6E96\
  \u51FA\u529B\uFF08stdout\uFF09\u3092\u3054\u3061\u3083\u3054\u3061\u3083\u306B\u3057\
  \u306A\u3044\u3088\u3046\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.779488-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u3067\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u306B\u66F8\
  \u304D\u8FBC\u3080\u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u901A\
  \u5E38\u306E\u7D50\u679C\u3068\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\
  \u533A\u5225\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u3053\u3068\u3092\u53EF\
  \u80FD\u306B\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u554F\u984C\u306E\u30B7\
  \u30B0\u30CA\u30EA\u30F3\u30B0\u3068\u30C7\u30D0\u30C3\u30B0\u306B\u4E0D\u53EF\u6B20\
  \u3067\u3042\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u4E3B\u8981\u306A\u30C7\
  \u30FC\u30BF\u3084\u7D50\u679C\u3092\u3057\u3070\u3057\u3070\u6301\u3064\u6A19\u6E96\
  \u51FA\u529B\uFF08stdout\uFF09\u3092\u3054\u3061\u3083\u3054\u3061\u3083\u306B\u3057\
  \u306A\u3044\u3088\u3046\u306B\u3057\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？
Haskellで標準エラー（stderr）に書き込むことは、プログラムが通常の結果とエラーメッセージを区別できるようにすることを可能にします。これは、問題のシグナリングとデバッグに不可欠であり、プログラムの主要なデータや結果をしばしば持つ標準出力（stdout）をごちゃごちゃにしないようにします。

## 方法:
Haskellでstderrに書き込むことは、基本ライブラリの`System.IO`モジュールを使って簡単に行えます。以下に基本例を示します：

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "これはエラーメッセージです。"
```

このプログラムのstderrへの出力は以下の通りです：

```
これはエラーメッセージです。
```

もし、より複雑なアプリケーションで作業している場合、またはログ（エラーを含む）に対してより良い制御が必要な場合は、サードパーティのライブラリを選択するかもしれません。一つの人気な選択肢は、Haskellプログラミングの`mtl`スタイルと統合する`monad-logger`です。こちらは`monad-logger`を使用した小さなスニペットです：

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "これはmonad-loggerを使ったエラーメッセージです。"
```

実行時に、`monad-logger`バージョンも同様にエラーメッセージを出力しますが、設定に応じて、タイムスタンプやログレベルなどのより多くのコンテキストが装備されています：

```
[Error] これはmonad-loggerを使ったエラーメッセージです。
```

両方の方法はstderrに書き込む目的を果たしますが、選択は大きくアプリケーションの複雑さとニーズに依存します。
