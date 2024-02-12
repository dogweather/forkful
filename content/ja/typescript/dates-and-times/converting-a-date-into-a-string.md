---
title:                "日付を文字列に変換する"
aliases: - /ja/typescript/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:54.610423-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？
日付を文字列に変換することは、日付データを読みやすい形式にするプロセスです。ログ記録、ユーザーインターフェース表示、または日付のフォーマットを標準化するためにプログラマーはこれを行います。

## How to:
## どうやって：
```TypeScript
// 日付を作成
const now = new Date();

// toLocaleStringを使って日付を文字列に変換（日本のロケール）
const str = now.toLocaleString('ja-JP');
console.log(str); // 出力例: "2023/4/1 12:00:00"

// toISOStringを使ってISO 8601フォーマットの文字列に変換
const isoStr = now.toISOString();
console.log(isoStr); // 出力例: "2023-04-01T03:00:00.000Z"
```

## Deep Dive:
## 詳細情報：
JavaScriptが1995年に登場して以来、日付と時刻の扱いは重要な要素でした。TypeScriptはJavaScriptのスーパーセットなので、日付を扱う方法も似ています。`Date` オブジェクトは多くのメソッドを提供していますが、`.toLocaleString()`, `.toString()`, `.toUTCString()`, `.toISOString()` などがあります。これらのメソッドは、それぞれ異なるケースに適用されます。例えば、`.toLocaleString()` はロケールに応じた形式で、`.toISOString()`はISO 8601形式で日付や時刻を表現します。

`.toLocaleString()`では、言語や国に応じて日付と時刻の形式を調整できます。これは国際化が要求されるアプリケーションに適しています。`.toISOString()` は、データベースやAPI通信で使われることが多く、タイムゾーンの偏りなしで日付を表現できるという利点があります。

代替手段としては、ライブラリを使用する方法があります。Moment.jsが長らくスタンダードでしたが、今日ではDay.jsやdate-fnsのような軽量でモダンなライブラリが推奨されます。

## See Also:
## 関連情報：
- MDN Web DocsにおけるDateオブジェクト: https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date
- Day.js: https://day.js.org/
- date-fns: https://date-fns.org/
- ISO 8601について: https://www.iso.org/iso-8601-date-and-time-format.html
