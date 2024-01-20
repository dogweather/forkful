---
title:                "現在の日付を取得する"
html_title:           "PowerShell: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？

現在の日付を取得することは、2022年6月16日のような詳細な日付と時刻を取得するプロセスを指します。プログラマはこれを行い、日付の記録、カレンダー機能の作成、または日付に基づいたロジックを実装するためです。

## どのようにするか：

以下は、TypeScriptで現在の日付を取得する簡単な方法です：

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

これを実行すると、こんな出力が得られます：

```TypeScript
2022-06-16T07:20:00.421Z
```
これは現在の日付と時刻をUTC（協定世界時）形式で表示しています。

## 詳細情報：

現在の日付を取得する方法は何世紀にもわたり編み出されてきました。コンピューターは、そもそもUNIXエポック（1970年1月1日00:00:00 UTC）からの秒数として時間を追跡します。TypeScriptのDateオブジェクトは、このシステムを利用して現在の日付を計算します。

TypeScript以外の言語では、日付と時刻を管理するためのさまざまな方法があります。戻り値形式、タイムゾーン、エポックなど、考慮すべきさまざまな要素があります。

TypeScriptの場合、新しいDateインスタンスを作成すると、オブジェクトは自動的にシステムの現在の日付と時刻に設定されます。そのため、現在の日付を取得するには特別なメソッドを呼び出す必要はありません。

## 関連リンク：

TypeScriptのDateオブジェクトについてさらに学びたい場合は、以下のリンクをご覧ください：

- [MDN Web Docs: Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript: Date](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)