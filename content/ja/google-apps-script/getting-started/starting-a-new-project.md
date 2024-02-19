---
aliases:
- /ja/google-apps-script/starting-a-new-project/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:04.558586-07:00
description: "Google Apps Script (GAS)\u3067\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\
  \u30AF\u30C8\u3092\u59CB\u3081\u308B\u3068\u306F\u3001Google\u30A8\u30B3\u30B7\u30B9\
  \u30C6\u30E0\uFF08Google Drive\u3001Docs\u3001Sheets\u306A\u3069\uFF09\u5185\u3067\
  \u30B9\u30AF\u30EA\u30D7\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u521D\u671F\u5316\u3057\
  \u3066\u3001\u30BF\u30B9\u30AF\u3092\u81EA\u52D5\u5316\u3057\u305F\u308A\u3001Google\u2026"
lastmod: 2024-02-18 23:08:54.523580
model: gpt-4-0125-preview
summary: "Google Apps Script (GAS)\u3067\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\
  \u30AF\u30C8\u3092\u59CB\u3081\u308B\u3068\u306F\u3001Google\u30A8\u30B3\u30B7\u30B9\
  \u30C6\u30E0\uFF08Google Drive\u3001Docs\u3001Sheets\u306A\u3069\uFF09\u5185\u3067\
  \u30B9\u30AF\u30EA\u30D7\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u521D\u671F\u5316\u3057\
  \u3066\u3001\u30BF\u30B9\u30AF\u3092\u81EA\u52D5\u5316\u3057\u305F\u308A\u3001Google\u2026"
title: "\u65B0\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u958B\u59CB"
---

{{< edit_this_page >}}

## 何となぜ？

Google Apps Script (GAS)で新しいプロジェクトを始めるとは、Googleエコシステム（Google Drive、Docs、Sheetsなど）内でスクリプトファイルを初期化して、タスクを自動化したり、Google Appsの機能を拡張したりすることを意味します。プログラマーは、ワークフローを合理化し、Googleサービスをプログラムで操作する、またはカスタムアドオンを作成するために、この旅をしばしば始めます。これにより、時間を節約し、Googleのインフラストラクチャの力を活用します。

## 方法:

Google Apps Scriptで新しいプロジェクトを開始するためには、いくつかの入口がありますが、最も直接的な方法に焦点を当てましょう：Google Driveからスクリプトを作成することです。

1. **Google Driveでのプロジェクトの作成**
   - Google Drive (drive.google.com)に移動します。
   - "+ 新規" > "その他" > "Google Apps Script"をクリックします。
   - 新しいスクリプトプロジェクトがエディタで開きます。デフォルトでは、サンプルの`myFunction`が含まれた`Code.gs`ファイルが含まれています。

2. **プロジェクトのセットアップ**
   - クリアにするためにプロジェクトの名前を変更します。左上の"無題のプロジェクト"をクリックし、意味のある名前を付けます。
   - `Code.gs`ファイルに簡単な関数を書いてみましょう：

```javascript
function helloWorld() {
  Logger.log('Hello, world!');
}
```

   - 関数を実行するには、再生ボタン (▶)の隣にあるドロップダウンから`helloWorld` 関数を選択してクリックします。これにより関数が実行されます。

3. **ログの表示**
   - `Logger.log`の出力を表示するには、"表示" > "ログ"に移動するか、`Ctrl + Enter`を押します。ログに"Hello, world!"と表示されるはずです。

おめでとうございます、Google Apps Scriptで新しいプロジェクトを成功裏に開始し、単純な関数を実行しました！

## 詳細解説

2009年頃にGoogle Apps Scriptが始まって以来、開発者と非開発者の両方にとって、Googleの幅広いサービスを自動化、拡張、構築するための強力でアプローチ可能なプラットフォームを提供しました。従来のプログラミング環境とは異なり、GASはGoogleエコシステム内での単純さと統合を提供し、外部サーバーや設定を必要としません。このサーバーレス実行モデルは、プロジェクトのデプロイメントと管理を大幅に簡素化します。

歴史的に、GASは実行環境と言語バージョンによって若干制限があり、しばしば現在のJavaScript標準よりも遅れていました。しかし、最近のアップデートにより、モダンなJavaScript構文（ECMAScript 2015+）がGASにもたらされ、現代的な開発慣行に慣れている開発者にとってより魅力的になりました。

GASはGoogleサービスとの対話に独特の地位を占めていますが、より集中的または特定のニーズに対応するための代替アプローチがあります。例えば、Google Cloud FunctionsやGoogle Cloud Platform (GCP)は、複雑なワークフローの処理、大規模なデータセットの処理、外部APIとの統合など、より堅牢でスケーラブルなソリューションを提供します。これらのプラットフォームは、さまざまな言語（例：Python、Go、Node.js）でのプログラミングを可能にし、より多くの計算資源を提供します。

それでも、Googleアプリに密接に関連したタスク、自動化、およびこのエコシステム内での迅速な開発に関して、Google Apps Scriptは使いやすさと統合の深さの点で他に類を見ないツールです。Google Driveから直接アクセスでき、Googleサービスとのシームレスな接続を提供するため、Sheets、Docs、Forms、その他のGoogleアプリケーションの機能を拡張したいと考えているプロジェクトにとって、実践的な選択肢です。
