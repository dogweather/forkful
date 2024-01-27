---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラムでテキストファイルを書くことは、情報を永続的に保存する行為です。データのエクスポート、ログの生成、またはシンプルなデータの交換によく使われます。

## How to: (方法)
Elmには直接ファイルシステムにアクセスする機能はありませんが、ElmからJavaScriptに情報を渡してファイルを作成することができます。以下はその例です。

```Elm
port module Main exposing (..)

-- ポートを定義
port saveFile : String -> Cmd msg

-- テキストデータをJavaScriptに送る関数
saveTextFile : String -> Cmd msg
saveTextFile text =
  saveFile text

-- JavaScriptでファイルをダウンロードさせるコード例
-- (HTMLファイル内のJavaScriptスクリプト)
<script>
  var app = Elm.Main.init();
  app.ports.saveFile.subscribe(function (text) {
    var blob = new Blob([text], {type : 'text/plain;charset=utf-8'});
    saveAs(blob, "file.txt");
  });
</script>
```

## Deep Dive (深掘り)
Elmは2012年に作られ、安全でメンテナンスしやすいウェブアプリケーションを実現するために設計されています。ファイルシステムへの直接アクセスはサポートしていないため、外部のJavaScriptと連携してファイルを生成するポート（`port`）と呼ばれる機構を利用します。Elmは`elm/file`パッケージを通じてファイルを読む機能を提供していますが、書く機能はブラウザのセキュリティ制約により直接ではできません。

## See Also (関連情報)
- Elmの公式ガイド: https://guide.elm-lang.org/
- JavaScriptとElmのポートについての詳細な説明: https://guide.elm-lang.org/interop/ports.html
- ファイル保存のJavaScriptライブラリ`FileSaver.js`: https://github.com/eligrey/FileSaver.js/
