---
title:                "パターンにマッチする文字を削除する"
html_title:           "Gleam: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
文字列から特定のパターンに一致する文字を削除するのか？それは、データの整理や加工を行う上で非常に便利な方法です。例えば、電話番号から-や()などの特殊文字を削除することで、データの統一性を保つことができます。

## 方法
```Gleam
// プログラムの宣言
import gleam/string

// 文字列の削除
let phone_number = "123-456-7890"
let cleaned_number = string.filter((char) => char != "-" , phone_number)
```

上記のコードを実行すると、`cleaned_number`には`1234567890`という数字のみが残されます。パターンに一致する文字を削除することで、不要な文字を取り除くことができます。

## ディープダイブ
この方法は、文字列だけでなくリストやタプルなどのデータ構造でも同様に使用することができます。また、パターンに一致する文字だけでなく、特定の条件を満たす文字を削除することもできます。詳細な使い方は[Gleamの公式ドキュメント](https://gleam.run/documentation/)を参照してください。

## 参考になるリンク
- [Gleamの公式ドキュメント](https://gleam.run/documentation)
- [Gleamのソースコード](https://github.com/gleam-lang/gleam)