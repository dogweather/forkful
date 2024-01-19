---
title:                "文字列を大文字にする"
html_title:           "Javascript: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 文字列を大文字にする: なぜとどうやって行うかについて

## 何となぜ？
文字列を大文字にするとは、全ての文字を大文字（例：HELLO WORLD）に変換することを意味します。プログラマーは、ユーザ入力の正規化、コードの一貫性を保つため、あるいは視覚的な強調のためにこれを行います。

## どのように実行するか：
``` Javascript
let message = 'hello world';
let upperCaseMessage = message.toUpperCase();
console.log(upperCaseMessage); // 出力: HELLO WORLD
```
このコードは、小文字のメッセージを大文字に変換します。

## Deep Dive
1. **歴史的な文脈:**  `toUpperCase()` メソッドは、初めてJavaScriptが作成された時点からありました。これは、開発者が文字列を大文字に簡単に変換できるように設計されています。

2. **代替案:** JavaScript以外にも、他の多くのプログラミング言語には同様の機能があります。例えば、Pythonでは `upper()`、C#では `ToUpper()` を使用します。

3. **実装の詳細:** `toUpperCase()` の内部では、文字列内のすべての小文字Unicode文字がそれぞれの大文字Unicode相当物にマッピングされます。

## 関連するリンク：
1. [MDN- toUpperCase()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
2. [Java - toUpperCase() Method](https://www.w3schools.com/java/ref_string_touppercase.asp)
3. [Python - upper() Method](https://www.w3schools.com/python/ref_string_upper.asp)