---
title:                "ウェブページのダウンロード"
html_title:           "Haskell: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何がそんなに面白いの？

ウェブページをダウンロードすることは、インターネット上でデータを入手するために一般的に使用されている方法です。プログラマーにとって、ウェブページをダウンロードすることは、データの収集や処理に役立ちます。

## 方法：

```Haskell
{-# LANGUAGE OverloadedStrings #-}

-- ライブラリをインポート
import Network.HTTP.Simple

-- 指定したURLからウェブページをダウンロード
getResponse :: IO ()
getResponse = do
    response <- httpLBS "https://www.google.com"
    putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    print $ getResponseBody response
```
出力：

```Haskell
Status code: 200
<!doctype html><html itemscope="" itemtype="http://schema.org/WebPage" lang="ja"><head><meta content="Google" itemprop="name"/><meta content="Google.co.jp は すぐにアクセスできる、無料のウェブサイトです。インターネットをもっと楽しく、便利に使いましょう。" name="description"><meta content="nocache" name="robots"><meta content="text/html; charset=UTF-8" http-equiv="Content-Type"><meta content="width=device-width,initial-scale=1" name="viewport"><title>Google</title><script nonce="kFM1USUziR6d5foEHDRKsg==">(function(){var aa=aa||{};...
```

## 深入り：

ウェブページのダウンロードは、HTTPプロトコルを使用して行われます。このプロトコルは、クライアント（ウェブブラウザやプログラム）がサーバーとやり取りするためのルールを定めたものです。HTTPプロトコル以外にも、FTPやSFTPなどのプロトコルを使用してダウンロードすることもできます。

## 関連情報：

[HTTPリクエストを取り扱うHaskellライブラリ](https://hackage.haskell.org/package/http-client)