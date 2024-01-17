---
title:                "文字列の先頭を大文字化する"
html_title:           "TypeScript: 文字列の先頭を大文字化する"
simple_title:         "文字列の先頭を大文字化する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何が:
文字列をキャピタライズするとは何かを説明し、プログラマーがそれを行う理由を説明します。

文字列をキャピタライズするとは、文字列の最初の文字を大文字にすることを指します。プログラマーは、見やすさや一貫性のために、しばしば文字列をキャピタライズすることがあります。

## 方法:

```TypeScript
const str = "hello world";
const capitalizedStr = str.charAt(0).toUpperCase() + str.slice(1);
console.log(capitalizedStr); // Output: "Hello world"
```

```TypeScript
const capitalize = (str: string) => {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

const str = "こんにちは世界";
const capitalizedStr = capitalize(str);
console.log(capitalizedStr); // Output: "こんにちは世界"
```

## 深く潜れ:

キャピタライズすることの歴史的な文脈、代替手段、そして実装の詳細について説明します。

キャピタライズすることは、主に見やすさや一貫性のために行われてきました。人間の目は、大文字の方が小文字よりもすぐに認識できるため、文字列をキャピタライズすることでより読みやすくなります。また、多くのプログラミング言語では、文字列に対してキャピタライズする関数が用意されています。

キャピタライズする代替手段としては、CSSのtext-transformプロパティを使用することができます。このプロパティを使用することで、HTML上のテキストを容易に大文字に変換することができます。

実装の詳細では、上記のコード例で使用したcharAt()、toUpperCase()、そしてslice()というメソッドについて説明します。charAt()は、指定された位置にある文字を返すメソッドであり、toUpperCase()は文字列を大文字に変換するメソッドです。slice()は、指定された位置から終了位置までの文字列を返すメソッドです。

## 他に見る:

- [JavaScript string methods: capitalize, upper/lowercase, and more](https://zellwk.com/blog/dynamic-case-changing-string-capitalize-ucfirst-upcase/)
- [TypeScript string methods](https://www.typescriptlang.org/docs/handbook/2/types-from-types.html#keyof-type-operators)