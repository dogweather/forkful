---
title:                "文字列を大文字にする"
aliases: - /ja/typescript/capitalizing-a-string.md
date:                  2024-02-03T19:06:48.449883-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
文字列を大文字化するとは、与えられた文字列の最初の文字を小文字から大文字に変更する操作で、通常、文字列の残りの部分は変更されません。この操作は一般的に、固有名詞や文の始まりがテキスト処理において文法規則に従うようにするために使用され、出力をプロフェッショナルで読みやすいものにします。

## 方法:

TypeScriptはJavaScriptのスーパーセットであるため、純粋なJavaScriptのアプローチから、より複雑または特定のユースケースのためにサードパーティのライブラリを利用する方法まで、文字列を大文字化するさまざまな方法を提供します。

**純粋なJavaScriptアプローチ:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// サンプル出力:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

この方法は直感的で、`charAt()`メソッドを使用して文字列の最初の文字にアクセスし、`toUpperCase()`でそれを大文字に変換します。その後、`slice(1)`メソッドで文字列の残りの部分を取得し、変更せずに残します。

**Lodashライブラリを使用する:**

[Lodash](https://lodash.com/)ライブラリをすでに使用しているプロジェクトでは、その`_.capitalize`関数を利用して、より少ないボイラープレートコードで同じ結果を得ることができます。

まず、Lodashをインストールします：

```bash
npm install lodash
```

次に、TypeScriptファイルで使用します：

```typescript
import * as _ from 'lodash';

// サンプル出力:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

注：Lodashの`_.capitalize`メソッドは、文字列の残りの部分を小文字に変換するため、常に望ましいわけではありません。

**正規表現を使用する:**

正規表現を使用すれば、文字列の最初の文字を大文字化する、特に文字列の各単語の最初の文字を大文字化する必要がある場合に、簡潔な方法を提供できます。

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// サンプル出力:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

この方法は、任意の単語境界に続く英数字文字(`\b\w`)を検索してそれぞれを大文字にする`replace()`関数を使用します。タイトルや見出しに特に便利です。
