---
date: 2024-01-26 01:08:08.818320-07:00
description: "\u4F7F\u3044\u65B9: \u6B74\u53F2\u7684\u306B\u898B\u3066\u3001\u30ED\
  \u30B0\u306F\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u81EA\u4F53\u304C\u59CB\u307E\
  \u3063\u305F\u9803\u304B\u3089\u5B58\u5728\u3057\u3066\u3044\u307E\u3059\u3002\u30BD\
  \u30D5\u30C8\u30A6\u30A7\u30A2\u306E\u305F\u3081\u306E\u8239\u9577\u306E\u30ED\u30B0\
  \u306E\u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\u6614\u306F\u51FA\u529B\u3084\
  \u30C6\u30EC\u30BF\u30A4\u30D7\u6A5F\u304C\u4F7F\u308F\u308C\u3066\u3044\u307E\u3057\
  \u305F\u304C\u3001\u73FE\u5728\u3067\u306F\u30D5\u30A1\u30A4\u30EB\u3084\u6D17\u7DF4\
  \u3055\u308C\u305F\u30ED\u30B0\u7BA1\u7406\u30B7\u30B9\u30C6\u30E0\u304C\u5168\u3066\
  \u3067\u3059\u3002 PowerShell\u306E\u6DF1\u3044\u3068\u3053\u308D\u3067\u4F5C\u696D\
  \u3092\u3057\u3066\u3044\u308B\u3068\u304D\u3001`Write-\u2026"
lastmod: '2024-04-05T22:50:56.335403-06:00'
model: gpt-4-1106-preview
summary: "\u6B74\u53F2\u7684\u306B\u898B\u3066\u3001\u30ED\u30B0\u306F\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u81EA\u4F53\u304C\u59CB\u307E\u3063\u305F\u9803\u304B\
  \u3089\u5B58\u5728\u3057\u3066\u3044\u307E\u3059\u3002\u30BD\u30D5\u30C8\u30A6\u30A7\
  \u30A2\u306E\u305F\u3081\u306E\u8239\u9577\u306E\u30ED\u30B0\u306E\u3088\u3046\u306A\
  \u3082\u306E\u3067\u3059\u3002\u6614\u306F\u51FA\u529B\u3084\u30C6\u30EC\u30BF\u30A4\
  \u30D7\u6A5F\u304C\u4F7F\u308F\u308C\u3066\u3044\u307E\u3057\u305F\u304C\u3001\u73FE\
  \u5728\u3067\u306F\u30D5\u30A1\u30A4\u30EB\u3084\u6D17\u7DF4\u3055\u308C\u305F\u30ED\
  \u30B0\u7BA1\u7406\u30B7\u30B9\u30C6\u30E0\u304C\u5168\u3066\u3067\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

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
