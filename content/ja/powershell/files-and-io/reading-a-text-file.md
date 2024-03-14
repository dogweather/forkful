---
date: 2024-01-20 17:54:49.475777-07:00
description: "\u30D5\u30A1\u30A4\u30EB\u8AAD\u307F\u8FBC\u307F\u306F\u3001\u30C6\u30AD\
  \u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\
  \u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u306F\u3001\u8A2D\u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\u30C7\u30FC\u30BF\u306E\
  \u51E6\u7406\u3001\u307E\u305F\u306F\u30B3\u30F3\u30C6\u30F3\u30C4\u306E\u5206\u6790\
  \u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.461124-06:00'
model: gpt-4-1106-preview
summary: "\u30D5\u30A1\u30A4\u30EB\u8AAD\u307F\u8FBC\u307F\u306F\u3001\u30C6\u30AD\
  \u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\
  \u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u306F\u3001\u8A2D\u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\u30C7\u30FC\u30BF\u306E\
  \u51E6\u7406\u3001\u307E\u305F\u306F\u30B3\u30F3\u30C6\u30F3\u30C4\u306E\u5206\u6790\
  \u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

ファイル読み込みは、テキストファイルからデータを取得するプロセスです。プログラマは、設定の読み込み、データの処理、またはコンテンツの分析のためにこれを行います。

## How to: (方法：)

PowerShellでテキストファイルを読む最も簡単な方法を見てみましょう。

```PowerShell
# ファイル全体を一度に読む
$content = Get-Content -Path "example.txt"
Write-Output $content

# 各行を一行ずつ読む
Get-Content -Path "example.txt" | ForEach-Object {
    Write-Output $_
}
```

出力サンプル:

```
Hello, PowerShell!
今日は良い天気ですね。
Let's read files!
```

## Deep Dive (掘り下げ：)

プログラマがファイルを読む理由はたくさんあります。設定ファイル、ログファイル、あるいはCSVなどのデータファイルを扱うことが多いです。PowerShellの`Get-Content` コマンドレットは、Windows PowerShell 1.0の時からあり、簡単にファイルの内容を引っ張ることができます。ほかの方法には、.NETのクラス（例: `System.IO.StreamReader`）を使うこともできますが、`Get-Content` は使いやすく、PowerShellスクリプトにおいて一般的です。ある時、大きなファイルを扱う場合、パフォーマンスのために各行を逐次処理したほうがいいかもしれません。

## See Also (関連情報：)

- [about_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1) (英語)
- [Get-Content](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1) (日本語)
