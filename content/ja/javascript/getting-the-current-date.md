---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何これとその理由？

プログラマーが現在の日付を取得すること。なぜそうするのか？それはサーバー上の活動を追跡したり、ユーザーが操作した日時を把握したりするためです。

## 使い方

```JavaScript
let today = new Date();
let date = today.getFullYear()+'年'+(today.getMonth()+1)+'月'+today.getDate()+'日';
console.log(date);
```  
あなたがこれを実行すると、今日の日付が以下のように表示されます。

```JavaScript
"2022年4月23日"
```

## 深い情報

1. 歴史的背景: JavaScriptは、1995年にNetscapeによって開発されました。当初から日付と時間を扱うための機能が提供されていました。
2. 他のオプション: `Date.now()`は現在の日時をミリ秒で返します。この戻り値を特定のフォーマットに変換することも可能です。
3. 実装の詳細: JavaScriptの`new Date()`は、クライアントのブラウザのシステム設定を用いて日時を返します。そのため、地域や時間帯によって結果が異なる場合があります。

## 関連情報

JavaScriptについてのさらなる情報は、以下のリンクをご覧ください。
- [MDN Web Docs: Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript.Info: Date and time](https://javascript.info/date)