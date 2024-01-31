---
title:                "ロギング"
date:                  2024-01-26T01:08:08.818320-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/logging.md"
---

{{< edit_this_page >}}

## 記録(Logging)とは、そしてなぜ必要か？
ログとは、基本的にコードを通じてパンくずリストを残すことです。これによって、スクリプトが実際に実行されているときに何が起こっているのかを追跡することができます。プログラマーは、デバッグするため、アプリの動作を追跡するため、パフォーマンスを監視するため、そして不正行為を見張るためにログを使用します。

## 使い方:
以下は、スクリプトに基本的なログを挿入する方法についてです：

```PowerShell
# 簡単なログメッセージの作成
Write-Host "情報: スクリプトの処理を開始します。"

# ファイルへの書き込み
"情報: これはログに記録されたメッセージです。" | Out-File -Append myLog.log

# より詳細なログのための組み込みcmdletの使用
Start-Transcript -Path "./detailedLog.log"
Write-Output "警告: 何かが少し正しくありません。"
# ... あなたのスクリプトは何かする
Stop-Transcript

# detailedLog.logの出力
******************************
Windows PowerShell トランスクリプト開始
開始時刻: 20230324112347
ユーザー名  : PShellGuru@example.com
RunAsユーザー: PShellGuru@example.com
コンフィギュレーション名: 
マシン  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
ホストアプリケーション: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
プロセスID: 2024
PS バージョン: 7.1.2
```

これで、ログにはコードが何をしていたのかの一連の動作が記録されます。

## 深掘り:
歴史的に見て、ログはプログラミング自体が始まった頃から存在しています。ソフトウェアのための船長のログのようなものです。昔は出力やテレタイプ機が使われていましたが、現在ではファイルや洗練されたログ管理システムが全てです。

PowerShellの深いところで作業をしているとき、`Write-Host`は素早くて簡単ですが、テキストをコンソールに出力するだけで、記録を保持するには適していません。`Out-File`はテキストをファイルに簡単に投げる方法を提供しますが、本当に重要な情報を得るためには、入力、出力、その他全てをログに記録する`Start-Transcript`と`Stop-Transcript`を使用することをお勧めします。

代替手段？もしエンタープライズ環境で作業しているなら、Windowsイベントログを見たり、Logstashのようなソフトウェアを使用することを検討するかもしれませんが、日常のスクリプトではPowerShellのツールを使い続けるのが良いでしょう。実装にあたっては、賢くログを取ることを忘れないでください—少なすぎても無駄になるし、多すぎてもホワイトノイズになります。

## 参照:
以下をチェックして、PowerShellでのログ取りの全体像を掴みましょう：
