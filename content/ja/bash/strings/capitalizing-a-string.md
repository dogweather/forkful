---
title:                "文字列を大文字にする"
aliases:
- /ja/bash/capitalizing-a-string.md
date:                  2024-02-03T19:04:56.096508-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Bashで文字列を大文字化するとは、文字列の最初の文字を大文字に変換し、残りの文字列は変更せずに残すことを指します。この技術は、出力のフォーマット変更や、一部の文字列が読みやすさやスタイルの好みのために大文字で始まることを要求するコーディング規約に準拠するために一般的に使用されます。

## どうやって：

Bashには文字列を大文字化するための専用の組み込み関数はありませんが、パラメータ展開や`awk`のような外部ツールを使用してこのタスクを達成することができます。ここに、Bashで文字列を大文字化するいくつかの方法を示します：

**パラメータ展開を使用する：**

この方法は、シェル内で直接文字列を操作します。

```bash
str="hello world"
capitalized="${str^}"
echo "$capitalized"
```
出力：
```
Hello world
```

**`awk`を使用する：**

`awk`は、ほとんどのUnix系オペレーティングシステムで利用可能な、強力なテキスト処理ツールであり、文字列を大文字化するために利用することができます。

```bash
str="hello world"
echo "$str" | awk '{print toupper(substr($0, 1, 1)) tolower(substr($0, 2))}'
```
出力：
```
Hello world
```

**`sed`を使用する：**

より伝統的な方法として、`sed`は文字列の最初の文字を大文字にするために使用することができます。しかし、これは前述の方法に比べると少し複雑です。

```bash
str="hello world"
echo "$str" | sed 's/./\u&/'
```
出力：
```
Hello world
```

これらのスニペットは、Bashで文字列の最初の文字を大文字にする方法を示しており、テキストを操作する際のシェルスクリプティングの柔軟性を強調しています。
