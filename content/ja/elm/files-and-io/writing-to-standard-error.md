---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:20.976899-07:00
description: "\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u306B\u66F8\u304D\u8FBC\
  \u3080\u3053\u3068\u306F\u3001\u30E1\u30A4\u30F3\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\
  \u51FA\u529B\u304C\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u306B\u884C\u304F\u3053\
  \u3068\u3068\u306F\u5225\u306B\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3084\u8A3A\u65AD\u3092\u30EA\u30C0\u30A4\u30EC\u30AF\u30C8\u3059\u308B\u3053\u3068\
  \u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u7279\u306B\u51FA\u529B\u306E\u533A\u5225\u304C\u30C7\u30D0\u30C3\u30B0\u3084\
  \u76E3\u8996\u306B\u4E0D\u53EF\u6B20\u306A\u74B0\u5883\u3067\u3001\u30A8\u30E9\u30FC\
  \u51E6\u7406\u3068\u30ED\u30B0\u8A18\u9332\u3092\u3088\u308A\u6271\u3044\u3084\u3059\
  \u304F\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.031045-06:00'
model: gpt-4-0125-preview
summary: "\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u306B\u66F8\u304D\u8FBC\
  \u3080\u3053\u3068\u306F\u3001\u30E1\u30A4\u30F3\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\
  \u51FA\u529B\u304C\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u306B\u884C\u304F\u3053\
  \u3068\u3068\u306F\u5225\u306B\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3084\u8A3A\u65AD\u3092\u30EA\u30C0\u30A4\u30EC\u30AF\u30C8\u3059\u308B\u3053\u3068\
  \u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u7279\u306B\u51FA\u529B\u306E\u533A\u5225\u304C\u30C7\u30D0\u30C3\u30B0\u3084\
  \u76E3\u8996\u306B\u4E0D\u53EF\u6B20\u306A\u74B0\u5883\u3067\u3001\u30A8\u30E9\u30FC\
  \u51E6\u7406\u3068\u30ED\u30B0\u8A18\u9332\u3092\u3088\u308A\u6271\u3044\u3084\u3059\
  \u304F\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\
  ."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 何となぜ？

標準エラー（stderr）に書き込むことは、メインプログラムの出力が標準出力（stdout）に行くこととは別に、エラーメッセージや診断をリダイレクトすることについてです。プログラマーは、特に出力の区別がデバッグや監視に不可欠な環境で、エラー処理とログ記録をより扱いやすくするためにこれを行います。

## 方法:

Elmは主にウェブ開発を対象としており、伝統的なコマンドライン環境でのように直接stderrに書き込むという概念は同じ方法で適用されません。しかし、Node.jsや類似の環境で実行されるElmプログラムにおいては、JavaScriptとのインターロップをポートを使用して実現することが、類似の機能を達成するための鍵となるアプローチです。ここでは、それを設定する方法を示します：

Elmのコード（`Main.elm`）:
```elm
port module Main exposing (main)

import Browser

port errorOut : String -> Cmd msg

-- JSにエラーメッセージを送信するダミーの例関数
generateError : String -> Cmd msg
generateError message =
    errorOut message
    
main =
    generateError "This is an error message for stderr"
```

JavaScriptとのインターロップ（`index.js`）:
```javascript
const { Elm } = require('./Main.elm');

var app = Elm.Main.init();

app.ports.errorOut.subscribe((message) => {
  console.error(message);
});
```

このElmコードは、ElmからJavaScriptにメッセージを送信することを可能にするポート`errorOut`を定義しています。次に、JavaScriptコードで、このポートを通して送信されたメッセージをリッスンし、`console.error()`を使用してstderrにリダイレクトします。この方法により、それをサポートする環境で効果的にstderrに書き込むことができます。これは、ElmのJavaScriptとのインターロップ機能を活用することによって実現されます。

Node.jsのターミナルでのサンプル出力（`index.js`を実行した場合）:
```
This is an error message for stderr
```
