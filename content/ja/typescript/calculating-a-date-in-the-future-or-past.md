---
title:                "未来または過去の日付を計算する"
html_title:           "TypeScript: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何とその理由？

日付の計算は、現在から未来や過去の特定の日付を導き出すことを言います。プログラマーは、有効期限、予定タスクの計画、特殊なイベントの日付などを追跡するためにこれを実行します。

## やり方：

TypeScriptで未来または過去の日付を計算する最も簡単な方法は、Dateオブジェクトを使用することです。以下はその例です：

```TypeScript 
let date = new Date(); // 現在の日付と時間
date.setDate(date.getDate() + 5); // 5日後の日付
console.log(date);
```

出力：
```
출력 예시:
2022-06-12T07:20:00.000Z
```

過去の日付を計算するには、必要な日数を現在の日から減算するだけです。

```TypeScript
let date = new Date(); // 現在の日付と時間
date.setDate(date.getDate() - 3); // 3日前の日付
console.log(date);
```

出力：
```
출력 예시:
2022-06-04T07:20:00.000Z
```

## 深掘り：

### 歴史的文脈 

初期のプログラミング言語では、日付操作のための組み込みオプションがなかったため、プログラマー自身がカスタムコードを作成しなければなりませんでした。しかし、現在の言語、TypeScriptを含むJavaScript系の言語では、標準のDateオブジェクトが提供されています。

### 代替手段 

別の手段として、日付操作を扱うためのライブラリが多数存在します。例えば、`moment.js`や`date-fns`などがあります。これらのライブラリは日付の計算だけでなく、形式変換や日付の比較など各種の日付操作をより簡便に行う機能を提供します。

### 実装詳細 

TypeScriptでは、過去または未来の日付を計算するために`getDate()`と`setDate()`の2つのメソッドを使用します。前者は現在の日付を取得し、後者は新しい日付を設定します。

## 関連リンク：

- MDN Web Docs: [JavaScriptのDateオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- date-fns: [公式ドキュメンテーション](https://date-fns.org/)
- moment.js: [公式ドキュメンテーション](https://momentjs.com/)