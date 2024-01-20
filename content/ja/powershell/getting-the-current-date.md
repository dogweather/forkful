---
title:                "現在の日付を取得する"
html_title:           "PowerShell: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ?
日付を取得することは、現在の日付と時間を取得することを指します。プログラマは、ログ付け、日付依存の機能を制御、または時間に基づく計算を行うためにこれを行います。

## 実装方法:
現在の日付を取得するのはとても簡単です。以下は、具体的なコード例と出力です：

```PowerShell
$現在日 = Get-Date
Write-Output $現在日
```

このコードを実行すると、現在の日付と時間が表示されます。

## 深掘り:
### 1.歴史的なコンテキスト:
PowerShellはWindowsの組み込みスクリプトであり、UNIXのシェルと似たコマンドライン環境を提供します。`Get-Date` というコマンドは、このシステムの一部としてこの早い段階で存在しました。

### 2.他の代替方法:
別の方法としては、.NETの `DateTime` オブジェクトを直接利用することです：

```PowerShell
$現在日 = [DateTime]::Now
Write-Output $現在日
```

### 3.実装の詳細:
`Get-Date` コマンドレットは、単純に .NET の `DateTime` クラスの `Now` メソッドへのラッパーです。それにより、現在の日付と時間のインスタンスが得られます。

## 関連資料:
PowerShellと`Get-Date`についてさらに学ぶには以下のリンクを参照してください：

1. [Microsoft公式ドキュメンテーション](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
2. [Windows PowerShellチュートリアル](https://www.tutorialspoint.com/powershell/index.htm)

以前のバージョンのPowerShellについて学ぶには、以下を参照してください：

3. [Windows PowerShell Wiki](https://en.wikipedia.org/wiki/PowerShell)

これらのリソースを通じて、あなたのPowerShellの知識を向上させてください。