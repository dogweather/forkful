---
title:                "テキストファイルの読み込み"
html_title:           "PowerShell: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 何＆なぜ？
テキストファイルを読み取るとは、テキスト形式で保存されたデータを読み取ることを意味します。プログラマーがこれを行う理由は、コンピューターのプログラムに必要なデータを処理するためです。

## 方法：
以下のように、PowerShellを使用してテキストファイルを読み取る方法を説明します。

```PowerShell
# テキストファイルを読み取る
Get-Content -Path C:\example.txt

# ヘッダーを含まないCSVファイルを読み取り、データを表示する
Import-Csv -Path C:\example.csv -Header Name, Age, Gender

# 読み取ったデータを別のファイルに書き込む
Get-Content -Path C:\example.txt | Set-Content -Path C:\newfile.txt
```

上のコードを実行すると、テキストファイルの内容がコンソールに表示されます。

## 深堀り：
テキストファイルを読み取る方法には、他にも様々なアプローチがあります。古いバージョンのPowerShellでは、```Get-Content```コマンドレットはなく、```cat```コマンドが使用されていました。また、他のプログラミング言語でも同様の機能を持つコマンドが存在します。

テキストファイルを読み取る際には注意が必要です。ファイルのエンコーディングや改行コードなど、ファイルの形式に応じて適切なコマンドを使用する必要があります。

## 関連情報：
- [PowerShell公式ドキュメント](https://docs.microsoft.com/en-us/powershell/) 
- [PowerShellテキストファイル関連コマンドレットの詳細説明](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/?view=powershell-7)
- [How-To Geek: How to Read and Write to Text Files with PowerShell](https://www.howtogeek.com/50236/how-to-read-and-write-to-text-files-with-powershell/)