---
title:                "C#: テキストの検索と置換方法"
simple_title:         "テキストの検索と置換方法"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換を行う理由はさまざまです。テキスト処理の効率を上げるために、あるいは大規模なテキストファイル内でのデータの置換を行うためになど、さまざまな目的で検索と置換を行うことができます。

## 方法

テキストの検索と置換は、プログラミング言語C#を使って比較的簡単に行うことができます。まず、検索するテキストを指定し、次に置換するテキストを指定することで、テキストの置換を行うことができます。

例えば、あるファイル内の特定の文字列を別の文字列に置換するには、以下のようなコードを使うことができます。

```C#
// 検索する文字列
string search = "こんにちは";

// 置換する文字列
string replace = "Hello";

// ファイルの内容を読み込む
string fileContent = File.ReadAllText("sample.txt");

// 検索と置換を行う
string newFileContent = fileContent.Replace(search, replace);

// 新しいファイルを作成し、内容を書き込む
File.WriteAllText("new_sample.txt", newFileContent);
```

実行すると、元のファイル内の「こんにちは」という文字列が「Hello」に置換された新しいファイルが生成されます。

## ディープダイブ

テキストの検索と置換は、複雑な検索条件を指定することも可能です。例えば、特定の単語が含まれる行のみを対象に置換することもできます。また、正規表現を使用することで、より高度な検索と置換を行うこともできます。

さらに、C#には検索と置換を行うための便利なメソッドやライブラリがたくさんあります。これらを使用することで、より高速かつ効率的にテキストの検索と置換を行うことができます。

## 関連リンク

- [C# ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/csharp/)
- [正規表現チュートリアル](https://www.tohoho-web.com/ex/regex.html)
- [string.Replace メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.replace?view=net-5.0)