---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となぜ？

検索と置換は、コード内の特定のテキストを見つけて新しいテキストで置き換えるプログラミングの一部です。この操作は、誤字の修正、特定の語彙の統一、またはコードのリファクタリングに便利です。

## 方法

この例では "Hello world" という文字列から "world" を "Japan" に置換します. 

```C#
string text = "Hello world";
string searchText = "world";
string replaceText = "Japan";

string newText = text.Replace(searchText, replaceText);
Console.WriteLine(newText);
```
このコードを実行すると、結果は "Hello Japan" になります。

## 詳細

**歴史的な文脈:** 比較的単純な操作だが, プログラミングの対話形式やXML プロセッサー, データベースのクエリ言語などの多くのコンテキストで使われてきました.

**代替:** `Regex.Replace` メソッドは、正規表現を使用して複雑な検索と置換を行う場合の代替手段として存在します. 

```C#
string pattern = @"\bworld\b";
string input = "Hello world";
string replaceWith = "Japan";
string result = Regex.Replace(input, pattern, replaceWith);
Console.WriteLine(result);
```

**実装の詳細:** 'Replace'はC#言語の内部で達成されます. メモリに新しい文字列のスペースを割り当て、古い文字列を新しい空間にコピーし、特定のテキストを新しいテキストで置き換えます.

## 関連リンク

- [Microsoft公式ドキュメント - String.Replace メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.replace?view=net-5.0)
- [Microsoft公式ドキュメント - Regex.Replace メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)