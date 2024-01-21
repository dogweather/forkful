---
title:                "テキストファイルの読み込み"
date:                  2024-01-20T17:54:49.475777-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/reading-a-text-file.md"
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