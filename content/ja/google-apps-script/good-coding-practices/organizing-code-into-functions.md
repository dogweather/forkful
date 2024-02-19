---
aliases:
- /ja/google-apps-script/organizing-code-into-functions/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:42.547154-07:00
description: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u7DE8\u6210\u3059\u308B\u3068\
  \u306F\u3001Google Apps \u30B9\u30AF\u30EA\u30D7\u30C8\u306E\u30B3\u30FC\u30C9\u3092\
  \u8AD6\u7406\u7684\u306A\u30BB\u30B0\u30E1\u30F3\u30C8\u306B\u5206\u5272\u3057\u3001\
  \u305D\u308C\u305E\u308C\u304C\u7279\u5B9A\u306E\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\
  \u3059\u308B\u72EC\u7ACB\u3057\u305F\u30D6\u30ED\u30C3\u30AF\u306B\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8907\u96D1\
  \u306A\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u7406\u89E3\u3057\u3084\u3059\u304F\u3001\
  \u30C7\u30D0\u30C3\u30B0\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u3001\u30B3\
  \u30FC\u30C9\u306E\u53EF\u8AAD\u6027\u3001\u4FDD\u5B88\u6027\u3001\u304A\u3088\u3073\
  \u518D\u5229\u7528\u6027\u3092\u9AD8\u3081\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.528564
model: gpt-4-0125-preview
summary: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u7DE8\u6210\u3059\u308B\u3068\
  \u306F\u3001Google Apps \u30B9\u30AF\u30EA\u30D7\u30C8\u306E\u30B3\u30FC\u30C9\u3092\
  \u8AD6\u7406\u7684\u306A\u30BB\u30B0\u30E1\u30F3\u30C8\u306B\u5206\u5272\u3057\u3001\
  \u305D\u308C\u305E\u308C\u304C\u7279\u5B9A\u306E\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\
  \u3059\u308B\u72EC\u7ACB\u3057\u305F\u30D6\u30ED\u30C3\u30AF\u306B\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8907\u96D1\
  \u306A\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u7406\u89E3\u3057\u3084\u3059\u304F\u3001\
  \u30C7\u30D0\u30C3\u30B0\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u3001\u30B3\
  \u30FC\u30C9\u306E\u53EF\u8AAD\u6027\u3001\u4FDD\u5B88\u6027\u3001\u304A\u3088\u3073\
  \u518D\u5229\u7528\u6027\u3092\u9AD8\u3081\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u7D44\u7E54\u5316\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

コードを関数に編成するとは、Google Apps スクリプトのコードを論理的なセグメントに分割し、それぞれが特定のタスクを実行する独立したブロックにすることです。プログラマーは、複雑なスクリプトを理解しやすく、デバッグしやすくするため、コードの可読性、保守性、および再利用性を高めるためにこれを行います。

## 方法：

Google Apps ScriptはJavaScriptに基づいており、`function` キーワードを使用して関数を定義します。その後に一意の関数名、パラメータを含むことができる丸括弧 `()` 、および関数のコードブロックをカプセル化する波括弧 `{}` が続きます。基本的な例を以下に示します：

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Hello, ' + user + '!');
}

greetUser();
```

サンプル出力：

```
Hello, someone@example.com!
```

次に、Google Sheetsに関連するより実用的な例を考えてみましょう。ここでは機能を2つの関数に分けます。1つはシートの設定用、もう1つはデータでシートを埋めるためのものです。

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Sales Data');
  sheet.appendRow(['Item', 'Quantity', 'Price']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Sales Data');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// データの配列を初期化
var salesData = [
  ['Widgets', 15, 2.5],
  ['Gadgets', 8, 3.75]
];

// 関数を実行する
setupSheet();
populateSheet(salesData);
```

この例では、`setupSheet`はシートの準備を行い、`populateSheet`は販売データの配列を使ってシートに情報を入力します。これらの懸念事項を分離することで、コードがよりクリーンになり、変更に対してより適応しやすくなります。

## 深堀り

コードを関数に分ける概念は、Google Apps Scriptに特有のものではなく、ほぼすべてのプログラミング言語で推奨されている基本的なプログラミング練習です。歴史的には、関数は入力を出力にマッピングする数学的概念から進化し、これが構造化プログラミングの礎となりました。このアプローチは、モジュール性とコードの再利用を促進し、スクリプトの個々の部分をテストするための明確な道を提供します。

Google Apps ScriptはJavaScriptベースであるため、JavaScriptのファーストクラス関数の恩恵を受けます。それにより、関数を引数として渡したり、他の関数から返したり、変数に割り当てたりすることが可能になります。この機能は、コールバックや関数型プログラミングのような高度なパターンを可能にしますが、これらのパターンはGoogle Apps Script内のシンプルな自動化タスクには不必要な複雑さをもたらす可能性があります。

より大きなプロジェクトやより複雑なアプリケーションについては、開発者はアロー関数、非同期操作のためのasync/await、さらには静的型付けを提供するTypeScriptなど、JavaScriptの新しい機能を探求するかもしれません。特に、TypeScriptはGoogle Apps Scriptとして実行されるようにコンパイルでき、より堅牢な型チェックと高度なオブジェクト指向機能を求める開発者にとっての道を提供します。

しかし、Google Appsスイート内のほとんどのスクリプティングニーズには、示されたようなシンプルでよく組織された関数を使用することが、堅固な基盤を提供します。高度な機能を効率のために活用することと、保守と可読性のためのシンプルさを保つことの間は、常にバランスを取る行為です。
