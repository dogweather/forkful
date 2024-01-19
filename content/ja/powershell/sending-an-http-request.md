---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストの送信は、サーバーにアクセスし情報を取得するための一般的な働きをすることです。プログラマーはこれを用いることで、ウェブサイトからデータを取得したり、APIにインタラクションをしたりすることが可能になります。

## 方法：

PowerShellを使用してHTTPリクエストを送信するには、Invoke-WebRequestコマンドを使用します。

```PowerShell
# Get webpage content
$res = Invoke-WebRequest -Uri "https://www.example.com"

# Printing status code
$res.StatusCode
```
このコードは、指定したURLからページの内容を取得し、Httpステータスコードを表示します。

## より深く：

HTTPリクエストの送信は、1980年代から存在しているが、シンプル化され、一般的に使用されるようになったのは最近のことです。PowerShell、Python、Javaなど、ほとんどのプログラミング言語はこの機能をサポートしています。一方、それぞれのシナリオにより適した異なる方法が存在します。例えば、非同期のリクエストを送信する場合や特定のヘッダーを使用する必要がある場合などは、他のコマンドを使用することがあります。

## 参考：

以下は、HTTPリクエストの送信についてさらに学ぶためのリンク集です：

- PowerShell Documentation: 
  [https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)

- Microsoft API usage guide: 
  [https://docs.microsoft.com/ja-jp/azure/active-directory/develop/v2-oauth2-client-creds-grant-flow](https://docs.microsoft.com/ja-jp/azure/active-directory/develop/v2-oauth2-client-creds-grant-flow)

PowerShellでHTTPリクエストを活用することで、繰り返し手作業を行う代わりに自動化できるため、作業をより効率的に行えます。これは、今日のデータドリブンな世界において非常に重要なスキルとなっています。