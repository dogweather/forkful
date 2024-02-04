---
title:                "正規表現の使用"
date:                  2024-02-03T19:19:33.610742-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
正規表現、またはregexは、プログラミングにおける強力なパターンマッチング及び検索ツールです。プログラマーは、ユーザー入力の検証、テキストの検索、または文字列の操作などのタスクにregexを使用します。なぜなら、それは効率的で多様性に富んでいるからです。

## どのようにして：

TypeScriptに入ってみて、一般的なタスクに対してregexがどのように使用されているか見てみましょう。

```TypeScript
// メールアドレスのregexパターンを定義する
const emailPattern = /\S+@\S+\.\S+/;

// 文字列がメールパターンにマッチするかテストする
const email = "user@example.com";
console.log(emailPattern.test(email)); // 出力: true

// 文字列の数字を見つけて置換する
const replaceDigits = "Item 25 costs $30".replace(/\d+/g, '#');
console.log(replaceDigits); // 出力: "Item # costs $#"

// 文字列から特定の部分を抽出するためにキャプチャグループを使用する
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, month, day, year] = datePattern.exec(data) || [];
console.log(month, day, year); // 出力: "April" "10" "2021"
```

## 深掘り

1950年代に、数学者のスティーブン・クリーネは正規言語を表すモデルとして正規表現を記述しました。それは後にコンピュータサイエンスにおいて必須のものとなりました。時は流れ、現在ではテキストを扱うプログラミングにおいてregexは無くてはならないものです。

regexは文字列操作のためのスイスアーミーナイフですが、それに代わるものもあります。タスクの複雑さに応じて、`includes()`、`startsWith()`、`endsWith()`といった文字列メソッドや、ライブラリによる解析がより良いこともあります。例えば、複雑なJSON文字列をregexで解析するのは悪夢です。代わりにJSONパーサーを使いましょう。

実装に関しては、JavaScriptとTypeScriptのregexはECMAScript言語仕様に基づいています。内部では、エンジンはパターンを効率的にマッチさせるために状態機械を使用しています。regex操作は特に貧弱なパターンでの使用はパフォーマンスにおいてコストがかかることがあり、"災害的バックトラッキング"に注意することが重要です。

## 参照

- 正規表現についてのMDN Web Docs: [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- 正規表現のパターンのテストとデバッグのためのツールRegex101: [Regex101](https://regex101.com/)
- 深い理解のための"Mastering Regular Expressions"という本: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
