---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ？

ウェブページのダウンロードは、指定されたURLのHTML内容を取得することです。プログラマーはこれを行うことで、情報の収集やデータの解析が可能になります。

## 方法：

以下は、PowerShellを用いてウェブページをダウンロードするベーシックな例です。コードの短いながらも明確でわかりやすいデモンストレーションを提供します。

```PowerShell
$URL = "http://example.com"
$OutputFile = "download.html"
Invoke-WebRequest -Uri $URL -OutFile $OutputFile
```
このコードは指定されたURL（この場合は "http://example.com"）から内容をダウンロードし、"download.html"という名前のファイルに保存します。

## 深層部：

歴史的な背景を見ると、ウェブページのダウンロードはもともと手動でブラウザを通じて行われていました。しかし、プログラムを用いて自動化することで、より効率的なデータ収集が可能になりました。

代替手段としては、wgetやcurlなどの他のコマンドラインツールがありますが、PowerShellはWindows環境でネイティブに実行できるため、ここではそれを使用しています。

Invoke-WebRequestコマンドについて詳しく見ていくと、-Uriパラメータで指定したURLからHTMLを取得し、-OutFileパラメータで指定したファイル名で保存します。これにより、ダウンロードしたウェブページの内容を後から解析したり、オフラインで閲覧したりすることができます。

## 参考情報：

以下のリンクから、PowerShellの詳細やそれに関連するコマンドについて更に学ぶことができます。

- [公式PowerShellドキュメンテーション](https://docs.microsoft.com/ja-jp/powershell/)
- [Invoke-WebRequestコマンドの詳細](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/invoke-webrequest)