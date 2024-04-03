---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:42.547154-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u306FJavaScript\u306B\u57FA\u3065\
  \u3044\u3066\u304A\u308A\u3001`function` \u30AD\u30FC\u30EF\u30FC\u30C9\u3092\u4F7F\
  \u7528\u3057\u3066\u95A2\u6570\u3092\u5B9A\u7FA9\u3057\u307E\u3059\u3002\u305D\u306E\
  \u5F8C\u306B\u4E00\u610F\u306E\u95A2\u6570\u540D\u3001\u30D1\u30E9\u30E1\u30FC\u30BF\
  \u3092\u542B\u3080\u3053\u3068\u304C\u3067\u304D\u308B\u4E38\u62EC\u5F27 `()` \u3001\
  \u304A\u3088\u3073\u95A2\u6570\u306E\u30B3\u30FC\u30C9\u30D6\u30ED\u30C3\u30AF\u3092\
  \u30AB\u30D7\u30BB\u30EB\u5316\u3059\u308B\u6CE2\u62EC\u5F27 `{}`\u2026"
lastmod: '2024-03-13T22:44:41.452753-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u306FJavaScript\u306B\u57FA\u3065\u3044\u3066\u304A\u308A\
  \u3001`function` \u30AD\u30FC\u30EF\u30FC\u30C9\u3092\u4F7F\u7528\u3057\u3066\u95A2\
  \u6570\u3092\u5B9A\u7FA9\u3057\u307E\u3059\u3002\u305D\u306E\u5F8C\u306B\u4E00\u610F\
  \u306E\u95A2\u6570\u540D\u3001\u30D1\u30E9\u30E1\u30FC\u30BF\u3092\u542B\u3080\u3053\
  \u3068\u304C\u3067\u304D\u308B\u4E38\u62EC\u5F27 `()` \u3001\u304A\u3088\u3073\u95A2\
  \u6570\u306E\u30B3\u30FC\u30C9\u30D6\u30ED\u30C3\u30AF\u3092\u30AB\u30D7\u30BB\u30EB\
  \u5316\u3059\u308B\u6CE2\u62EC\u5F27 `{}` \u304C\u7D9A\u304D\u307E\u3059\u3002\u57FA\
  \u672C\u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\u3059\uFF1A."
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u7D44\u7E54\u5316\u3059\u308B"
weight: 18
---

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
