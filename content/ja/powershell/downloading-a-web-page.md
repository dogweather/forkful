---
title:                "ウェブページのダウンロード"
html_title:           "PowerShell: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## やってみよう！
こんにちは、みなさん！今日はPowerShellについてお話しします。PowerShellとは、Windowsでのシステム管理やタスク実行に使用されるスクリプト言語です。

## 何をし、なぜ？
ウェブページをダウンロードすることは、インターネット上の情報を取得することです。プログラマーは、さまざまな目的でウェブページをダウンロードします。例えば、データを収集したり、情報を解析したり、自動化したりするためです。

## 方法：
以下のコード例を使用して、ウェブページをダウンロードする方法を紹介します。
```
# Invoke-WebRequestコマンドレットを使用して、ウェブページをダウンロードする
$content = Invoke-WebRequest -Uri "https://www.example.com"

# ウェブページのソースコードを表示する
$content.Content
```
上記のコードを実行すると、"https://www.example.com"にあるウェブページのソースコードが表示されます。

## 詳細について：
ウェブページをダウンロードするためには、Invoke-WebRequestコマンドレットを使用します。このコマンドレットは、特定のURLからコンテンツを取得するために使用されます。また、ウェブページをダウンロードする方法としては、HTTPリクエストを直接送信する方法もありますが、PowerShellの場合はInvoke-WebRequestコマンドレットを使用する方が簡単です。

## 関連情報：
詳しい情報は、以下のリンクをご参照ください。
- Microsoft公式ドキュメント: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-6
- Invoke-WebRequestコマンドレットの詳細: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-6

では、今日はここまで！この記事がウェブページをダウンロードする方法についての参考になれば幸いです。また、次回の記事もお楽しみに！