---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:47.171699-07:00
description: "Google Apps Script\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\
  \u6570\u3092\u8AAD\u307F\u53D6\u308B\u3053\u3068\u306F\u3001Python\u3084Node.js\u306A\
  \u3069\u306E\u4F1D\u7D71\u7684\u306A\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u30A4\
  \u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3092\u6301\u3064\u30D7\u30ED\u30B0\u30E9\
  \u30DF\u30F3\u30B0\u8A00\u8A9E\u3068\u306F\u7570\u306A\u308A\u3001\u5C11\u3057\u8AA4\
  \u89E3\u3092\u62DB\u304F\u540D\u524D\u3067\u3059\u3002\u306A\u305C\u306A\u3089\u3001\
  Google Apps\u2026"
lastmod: '2024-02-25T18:49:39.619537-07:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\
  \u6570\u3092\u8AAD\u307F\u53D6\u308B\u3053\u3068\u306F\u3001Python\u3084Node.js\u306A\
  \u3069\u306E\u4F1D\u7D71\u7684\u306A\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u30A4\
  \u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3092\u6301\u3064\u30D7\u30ED\u30B0\u30E9\
  \u30DF\u30F3\u30B0\u8A00\u8A9E\u3068\u306F\u7570\u306A\u308A\u3001\u5C11\u3057\u8AA4\
  \u89E3\u3092\u62DB\u304F\u540D\u524D\u3067\u3059\u3002\u306A\u305C\u306A\u3089\u3001\
  Google Apps\u2026"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u8FBC\
  \u307F"
---

{{< edit_this_page >}}

## 何となぜ？

Google Apps Scriptでコマンドライン引数を読み取ることは、PythonやNode.jsなどの伝統的なコマンドラインインターフェースを持つプログラミング言語とは異なり、少し誤解を招く名前です。なぜなら、Google Apps Scriptは本質的にコマンドライン実行や引数の解析をサポートしていないためです。代わりに、コーダーはカスタム関数やURLパラメーターを使用してこのプロセスをシミュレートしながら、Webアプリや自動化されたタスクを実行して、ユーザー入力や事前定義されたパラメーターに基づいてスクリプト機能との動的なやり取りを可能にします。

## 方法：

Google Apps Scriptでコマンドライン引数を読み取るプロセスを模倣するためには、特にWebアプリの場合、クエリ文字列パラメーターを利用できます。ユーザーがWebアプリURLにアクセスするとき、`?name=John&age=30`のような引数を追加して、Apps Scriptコード内でこれらを解析できます。こうした設定方法を紹介します：

```javascript
function doGet(e) {
  var params = e.parameter; // クエリ文字列パラメーターを取得
  var name = params['name']; // 'name' パラメーターを取得
  var age = params['age']; // 'age' パラメーターを取得

  // サンプル出力：
  var output = "Name: " + name + ", Age: " + age;
  return HtmlService.createHtmlOutput(output);
}

// 例のURL: https://script.google.com/macros/s/your_script_id/exec?name=John&age=30
```

指定されたパラメータでURLにアクセスすると、スクリプトは次のような出力をします：

```
Name: John, Age: 30
```

このアプローチは、Webアプリで個性的なインタラクションを作成したり、プログラム的にスクリプトの実行を制御するのに役立ちます。

## 深掘り

伝統的なプログラミング言語の文脈で理解されるコマンドライン引数は、スクリプトやアプリケーションが実行時パラメーターを処理する能力を提供し、ユーザー入力や自動化されたプロセスに基づいて柔軟で動的なコード実行を可能にします。Google Workspaceエコシステム内で軽量アプリケーション開発のためのクラウドベースのスクリプト言語であるGoogle Apps Scriptは、本来、コマンドラインインターフェースを介して操作されるものではありません。代わりに、その実行は主にイベント駆動型またはApps ScriptやGoogle Workspace UIを通じて手動でトリガーされるか、URLパラメーターを擬似コマンドライン引数として解析できるWebアプリを介して行われます。

このアーキテクチャの違いを考えると、CLIを多用する言語のバックグラウンドから来たプログラマーは、Google Apps Scriptでタスクを自動化したり、アプリケーションを開発したりする際に、アプローチを調整する必要があります。伝統的なコマンドライン引数の解析の代わりに、Google Apps ScriptのWebアプリ機能やGoogleシートのカスタム関数を活用してインタラクティブなデータ処理を実施することが類似の目的に役立ちます。これは最初は制限のように思えるかもしれませんが、Google Workspaceアプリケーションのシームレスな統合と拡張に焦点を当てたGoogle Apps Scriptの目標に沿った、よりユーザーフレンドリーなインターフェイスやアクセスしやすいWebアプリケーションの開発を促進します。

CLI動作のより密なエミュレーションが最優先事項であるシナリオ（例えば、動的パラメーターを用いてタスクを自動化する場合）では、開発者は外部プラットフォームを活用してGoogle Apps Script Webアプリを呼び出し、URLを通じてパラメーターを渡す仮想の「コマンドライン」手法を検討できます。しかし、ネイティブのGoogle Apps Scriptプロジェクトに対しては、プラットフォームのイベント駆動型およびUI中心のモデルを受け入れることが、しばしばより直接的で維持しやすい解決策につながります。
