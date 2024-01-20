---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の長さを見つけるとは、コードに含まれる文字の数をカウントすることを指します。プログラマーがこれを行う理由は、特定のアルゴリズムの実行に結びつくデータの量を判断したり、入力の検証を行ったりするためです。

## どうやって:

以下に、文字列の長さを見つける方法を示すTypeScriptのコードスニペットを提供します。

```TypeScript
let str: string = "こんにちは、世界!";
let length: number = str.length;
console.log(length);
```

これはサンプル出力を以下に生成します。

```Output
8
```

## より深く掘り下げてみよう:

1. **歴史的背景**: この操作は、最初のプログラミング言語が誕生したときから存在しています。文字列長を知ることは、必要なストレージ空間の計算からユーザー入力の検証まで、さまざまなタスクを簡単にします。

2. **代替案**: TypeScriptでは、Array.prototype.lengthメソッドも使用できます。ただし、str.lengthは一般的に推奨される方法です。

3. **実装の詳細**: TypeScriptの場合、`.length`は文字列インスタンスのプロパティであり、文字列の長さを返します。これはUTF-16単位で文字数を返すため、全てのUnicode文字が1文字としてカウントされます。

## 参考資料:

- [Mozilla Developer Network's Guide on Strings](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Grammar_and_types#String)
- [StackOverflow: How do I get the length of a string in TypeScript?](https://stackoverflow.com/questions/43607288/how-do-i-get-the-length-of-a-string-in-typescript)