---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

---

## 何となぜ？

文字列の連結とは、２つ以上の文字列を1つの文字列に結合するプロセスのことを指します。プログラマーは、ユーザーが読むためのメッセージを生成したり、データを整形したりするために、この技術を利用しています。

---

## どうやって：

Elixirには `<>`演算子を使用して文字列を連結させる方法があります。

```Elixir
string1 = "こんにちは、"
string2 = "世界！"
string3 = string1 <> string2
IO.puts(string3)
```
このコードの出力は次のとおりです：
```Elixir
こんにちは、世界！
```
リスト型の文字列を連結するためには、`++`演算子を用いることができます。
```Elixir
list1 = ['こん', 'にちは、']
list2 = ['世', '界！']
list3 = list1 ++ list2
IO.puts(Enum.join(list3))
```
このコードの出力は次の通り：
```Elixir
こんにちは、世界！
```

---

## ディープダイブ：

Elixir の文字列連結の `<>` 演算子は、Elixirの祖先であるErlangから由来しています。この演算子は、ビット列を効果的に連結することができるため、大規模なデータ操作に非常に有用です。

また、リストの連結には`++`演算子を用います。これは、Erlangのリスト連結と同じで、元のリストの最後の要素まで走査する必要があるため、連結するリストが大きい場合は遅くなる可能性があります。

なお、Elixirでは文字列操作の実装には、効率的な文字処理のためにutf-8対応の2進パターンマッチングが利用されています。

---

## 関連リンク：

Elixirにおける文字列の操作についてより深く学びたい方は、以下のリンクをご覧ください。

- Elixir公式ドキュメンテーション（文字列）： https://elixir-lang.org/getting-started/io-and-the-file-system.html#iodots
- Elixir School（文字列と文字列操作）： https://elixirschool.com/jp/lessons/basics/strings/