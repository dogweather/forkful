---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付のパーシングは、文字列から日付形式への変換を指します。プログラマはこれを使って日付の入力を受け取り、行う変更を適用し、次の操作のために日付を使えるようにします。

## 方法：

TypeScriptを使った日付パーシングの一例を見てみましょう。

```TypeScript 
let date = new Date("2020-01-01");
console.log(date);
```

出力は次のようになります：

```TypeScript 
2020-01-01T00:00:00.000Z
```

この方式だと、年-月-日の形式で日付を簡単にパースできます。

## 深掘り：

日付のパースは開発の歴史の初めから存在しています。しかしその時間、多くのテクニックとツールが作られ、試されてきました。TypeScriptでは、組み込みの`Date`オブジェクトを使うことで簡単にパースを行うことができます。

しかし、他のライブラリーも存在し、特定の場合下ではより有用かもしれません。例えば、*Moment.js*や*date-fns*などはより細かい制御を提供し、複雑な日付計算が可能です。

TypeScriptの`Date`オブジェクトは、文字列を含むいくつかの入力を受け取り、ISO形式の日付に変換します。それがStringをDateに変換するプロセスの核心です。

## 参照：

- MDNの[Dateオブジェクトの解説](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)（日本語）
- [Moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)