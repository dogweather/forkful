---
title:                "パターンに一致する文字を削除する"
date:                  2024-01-20T17:42:11.928194-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字パターン削除は、決まったパターンに合致する文字を取り除くことです。不要なデータをクリアするか、入力をきれいに整形するためにプログラマはこれを行います。

## How to: (方法)
```gleam
import gleam/regex

fn delete_pattern(text: String, pattern: String) -> String {
  case regex.run(~re/pattern/, text) {
    Ok(matches) -> regex.replace(matches.at(0), text, "", global: true)
    Error(_) -> text
  }
}

// 例: 全ての数字を削除します
fn main() {
  let text = "123abc456def"
  let pattern = "[0-9]"
  let result = delete_pattern(text, pattern)
  assert result == "abcdef"
}

main()
```

## Deep Dive (深い潜水)
パターンマッチを利用する文字削除の概念は、Unixのgrepコマンドやsedエディタから発展してきました。他のアプローチには文字列をループして条件に一致するものを削除する方法がありますが、Gleamではregexモジュールを使って簡単かつ効果的に実行できます。正規表現エンジンは通常、Boyer-MooreやRegex-Directed Engineをより高速、効率的なマッチングに使います。

## See Also (参照)
- Regular expressions in programming: [https://en.wikipedia.org/wiki/Regular_expression](https://en.wikipedia.org/wiki/Regular_expression)
- Example uses of Unix grep and sed: [https://www.gnu.org/software/grep/manual/grep.html](https://www.gnu.org/software/grep/manual/grep.html), [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)