---
date: 2024-01-20 17:54:49.475777-07:00
description: "How to: (\u65B9\u6CD5\uFF1A) PowerShell\u3067\u30C6\u30AD\u30B9\u30C8\
  \u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u6700\u3082\u7C21\u5358\u306A\u65B9\u6CD5\
  \u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.461124-06:00'
model: gpt-4-1106-preview
summary: "PowerShell\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\
  \u3080\u6700\u3082\u7C21\u5358\u306A\u65B9\u6CD5\u3092\u898B\u3066\u307F\u307E\u3057\
  \u3087\u3046."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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
