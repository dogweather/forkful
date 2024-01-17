---
title:                "テキストの検索と置換"
html_title:           "C#: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? 「何をするのか」

検索と置換は、テキストの特定の部分を見つけて、別のテキストに置き換えることです。これは、プログラマーがコードやファイル内の特定の単語やパターンを素早く見つけて変更できるようにするために行われます。

## How to: 「やり方」

検索と置換は、C#で簡単に実装することができます。まず、次のように「Regex.Replace」メソッドを使用して、テキスト内の特定の単語を別の単語に置き換えます。

```C#
string text = "今日は晴れです";
string newText = Regex.Replace(text, "晴れ", "雨");
// newTextは「今日は雨です」になります
```

また、正規表現を使用して、テキスト内のパターンを置き換えることもできます。次の例では、パターン「\d\d\d\d」（４つの数字）を見つけて、置き換えます。

```C#
string text = "私の電話番号は1234-5678です";
string newText = Regex.Replace(text, @"\d\d\d\d", "1111");
// newTextは「私の電話番号は1111-5678です」になります
```

## Deep Dive 「深堀り」

検索と置換は、テキスト処理における基本的な操作です。これらは、テキストエディタやプログラムの内部で使用されることがあります。また、正規表現を使用することで、さらに柔軟な検索と置換が可能になります。

代替手段としては、テキストエディタやコマンドラインツールでの検索と置換機能があります。ただし、プログラム内で行うことで、より高度な処理が可能になります。

C#で検索と置換を行うには、正規表現の知識が必要です。正規表現は、パターンマッチングにおいて非常に強力なツールであり、他の言語やプログラムでも使用することができます。

## See Also 「参考」

- [C#で正規表現を使用した検索と置換の方法](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/how-to-search-strings-using-regular-expressions)
- [正規表現の基礎を理解する](https://www.ibm.com/support/knowledgecenter/SSCP65_5.8.5/com.ibm.help.tspj_5.8.5.doc/SPJUG_Help/Regex_Asym_RegExIntro.html)