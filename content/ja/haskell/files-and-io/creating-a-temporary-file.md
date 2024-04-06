---
date: 2024-01-20 17:40:53.967756-07:00
description: "How to: (\u65B9\u6CD5) \u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u53E4\
  \u304F\u304B\u3089\u4F7F\u308F\u308C\u3001\u30C7\u30A3\u30B9\u30AF\u306B\u4E00\u6642\
  \u7684\u306A\u30C7\u30FC\u30BF\u3092\u66F8\u3044\u3066\u4F7F\u3044\u307E\u3059\u3002\
  `System.IO.Temp` \u306F\u5B89\u5168\u306B\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\
  \u53D6\u308A\u6271\u3046\u305F\u3081\u306EHaskell\u30E9\u30A4\u30D6\u30E9\u30EA\u3067\
  \u3059\u3002\u95A2\u6570`withSystemTempFile`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.133368-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u53E4\u304F\u304B\
  \u3089\u4F7F\u308F\u308C\u3001\u30C7\u30A3\u30B9\u30AF\u306B\u4E00\u6642\u7684\u306A\
  \u30C7\u30FC\u30BF\u3092\u66F8\u3044\u3066\u4F7F\u3044\u307E\u3059\u3002`System.IO.Temp`\
  \ \u306F\u5B89\u5168\u306B\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u53D6\u308A\
  \u6271\u3046\u305F\u3081\u306EHaskell\u30E9\u30A4\u30D6\u30E9\u30EA\u3067\u3059\u3002\
  \u95A2\u6570`withSystemTempFile` \u306F\u30D5\u30A1\u30A4\u30EB\u3092\u81EA\u52D5\
  \u7684\u306B\u524A\u9664\u3059\u308B\u306E\u3067\u3001\u4E00\u6642\u30D5\u30A1\u30A4\
  \u30EB\u3092\u6383\u9664\u3059\u308B\u624B\u9593\u304C\u7701\u3051\u307E\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

## How to: (方法)
```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hClose)

-- 一時ファイルを生成して使用するサンプル
main :: IO ()
main = withSystemTempFile "mytemp.txt" $ \filePath handle -> do
    -- ファイルに内容を書き込む
    hPutStrLn handle "一時ファイルの中身だよ"
    -- 必要な操作をここで実行
    -- ...
    putStrLn $ "一時ファイル作成：" ++ filePath
    -- withSystemTempFile はファイルを自動的に閉じる
```

出力サンプル:
```
一時ファイル作成：/tmp/mytemp.txt123456
```

## Deep Dive (掘り下げ)
一時ファイルは古くから使われ、ディスクに一時的なデータを書いて使います。`System.IO.Temp` は安全に一時ファイルを取り扱うためのHaskellライブラリです。関数`withSystemTempFile` はファイルを自動的に削除するので、一時ファイルを掃除する手間が省けます。

他の方法としては `openTempFile` や `createTempDirectory` などがあります。これらは一時ファイルやディレクトリをより細かくコントロールするために使われますが、後始末が必要になる場合があります。

実装の詳細では、`withSystemTempFile` や類似の関数は内部的に一意なファイルネームを生成し、アトミックにファイルを作成することで競合を避けます。安全性と一意性は一時ファイルを作る際の重要な特性です。

## See Also (関連情報)
- Haskellの公式ドキュメント: https://www.haskell.org/documentation/
- `System.IO.Temp` モジュールに関するドキュメント: https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html
- IO ライブラリに関する情報: https://hackage.haskell.org/package/base/docs/System-IO.html
