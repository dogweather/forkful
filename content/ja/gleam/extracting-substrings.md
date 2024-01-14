---
title:    "Gleam: 部分文字列の抽出"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## なぜ

サブストリングを抽出することは、特定の言語やパターンを含むテキストから必要な情報を取得するために役立ちます。例えば、特定の単語を含む文章のみを抽出したり、文字列の一部を取り出すことができます。

## 方法

サブストリングを抽出するには、Gleamの組み込み関数である`String.slice`を使用します。例えば、次のように書くことで、文字列から指定した開始位置と終了位置の間の文字列を取り出すことができます。

```
Gleam String.slice("Hello world", 0, 5)
```

この場合、出力は`Hello`となります。また、終了位置を指定しない場合は、文字列の最後までを抽出することができます。

```
Gleam String.slice("Hello world", 6)
```

この場合、出力は`world`となります。さらに、正規表現を使用することで、特定のパターンを含む文字列を抽出することもできます。

## ディープダイブ

`String.slice`は、文字列を抽出する際に一部の文字をスキップするオプションも提供しています。例えば、次のように書くことで、2文字目から最後までの文字列を取り出すことができます。

```
Gleam String.slice("Hello world", 2)
```

出力は`llo world`となります。さらに、マッチした部分文字列を他の文字列と置き換えることもできます。

```
Gleam String.slice("Hello world", 0, 5, "こんにちは")
```

この場合、出力は`こんにちは world`となります。

## また見る

- Gleamドキュメント：https://gleam.run/
- 正規表現を使用した抽出のさらなる例：https://gleam.run/docs/std/regex#extract
- `String.slice`の詳細な説明：https://gleam.run/docs/std/string#slice