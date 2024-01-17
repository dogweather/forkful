---
title:                "サブストリングの抽出"
html_title:           "Gleam: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

Gleamでサブストリングを抽出する方法

## What & Why?
サブストリングを抽出するとは、文字列の一部分を切り出すことです。プログラマーは、文字列から必要な情報を取り出し、必要な操作を行うために、サブストリングを抽出する必要があります。

## How to:
```
Gleam.String.substring("Hello World", 0, 5)   # 出力: "Hello"
```

```
Gleam.String.substring("Hello World", 6, 11)  # 出力: "World"
```

```
Gleam.String.substring("Hello World", 6)      # 出力: "World"
```

## Deep Dive:
サブストリングの概念は古くから存在し、文字列処理において非常に重要な役割を果たしています。サブストリングを取り出す方法としては、他にも様々なアプローチがありますが、Gleamでは標準ライブラリを利用することで簡単に実装することができます。

サブストリングを抽出する方法には、他にも文字列から特定の文字列やパターンを検索し抽出する方法があります。また、サブストリングを操作するための様々なライブラリも存在します。

Gleamでは、文字列は不変(immutable)であるため、サブストリングを抽出する際には新しい文字列が作成されます。

## See Also:
- [Gleamのドキュメント](https://gleam.run/documentation/)
- [サブストリングについての詳細な記事](https://qiita.com/hiroga/items/7d2030567413b6697867)