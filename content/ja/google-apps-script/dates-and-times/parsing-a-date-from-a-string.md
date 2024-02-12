---
title:                "文字列から日付を解析する"
aliases:
- /ja/google-apps-script/parsing-a-date-from-a-string/
date:                  2024-02-01T21:57:36.324237-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付を解析する"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

文字列から日付を解析することは、日付を表すテキストを日付オブジェクトに変換し、プログラマが比較、算術、フォーマットなどの日付関連の操作を実行できるようにすることを意味します。これは、ユーザ入力の処理、外部ソースからのデータ処理、さまざまな形式の日付の管理、特にスケジューリング、データ分析、あるいは時間ベースの記録を含むアプリケーションにおいて、不可欠です。

## どのようにして：

Google Apps Scriptでは、JavaScriptに基づいていますが、文字列から日付を解析するためのいくつかのアプローチがあります。以下は、ネイティブのJavaScriptメソッドとGoogle Apps Scriptのユーティリティを使用した例です。

**`new Date()` コンストラクタを使う:**

Google Apps Scriptで文字列を日付に解析する最も単純な方法は、`Date`オブジェクトのコンストラクタを使用することです。ただし、Date.parse() メソッドによって認識される形式の日付文字列が必要です（例えば、YYYY-MM-DD）。

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)をログに記録
```

**`Utilities.parseDate()` を使う:**

カスタム日付フォーマットに対してより柔軟性を持たせるために、Google Apps Scriptは `Utilities.parseDate()` を提供しています。このメソッドを使うと、日付フォーマット、タイムゾーン、およびロケールを指定できます。

```javascript
const dateString = '01-04-2023'; // DD-MM-YYYY
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // スクリプトのタイムゾーンに応じて Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)をログに記録
```

注: `Utilities.parseDate()` はより多くの制御を提供しますが、スクリプトのタイムゾーンに基づいてその挙動が変わる可能性があるので、アプリケーションが複数の地域で日付を扱う場合には、タイムゾーンを明示的に指定することが重要です。

## 深掘り

プログラミング言語での日付解析は、歴史的にさまざまな日付フォーマットやタイムゾーンの複雑さのために、課題に直面してきました。Google Apps Scriptのアプローチは、主にJavaScriptから派生しており、直接的な`Date`オブジェクトとより汎用性の高い`Utilities.parseDate()`関数の両方を提供することで、これを単純化しようとしています。しかしながら、各方法には制限があります；例えば、文字列を使用した`Date`コンストラクタに頼ると、日付フォーマットの解釈が異なるため、異なる環境での不一致が発生します。一方で、`Utilities.parseDate()`は、フォーマット、タイムゾーン、およびロケールの明確な理解が必要であり、特定のニーズに対してはやや複雑ですが、より信頼性が高いです。

Moment.jsのような代替のライブラリやサービス（新しいプロジェクトではLuxonを推奨）は、より豊富な機能やより良いゾーン処理を提供し、これらの課題に対処しています。ただし、外部ライブラリが制限されるGoogle Apps Scriptのコンテキストでは、組み込みのメソッドを効果的に理解して活用することが重要になります。他の言語から来たプログラマは、Google Apps Scriptでの日付処理のニュアンスを特有の挑戦と見なすかもしれませんが、利用可能なツールの深い理解とアプリケーションのグローバルな性質を慎重に考慮すれば、堅牢な日付解析を達成することができます。
