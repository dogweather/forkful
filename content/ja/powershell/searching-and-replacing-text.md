---
title:                "テキストの検索と置換"
html_title:           "PowerShell: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何？どうして？

検索と置換とは、テキストの内容を変更することを意味します。プログラマーがこれを行うのは、コード内の特定のキーワードや表現を一度に見つけ出したり、同時に置き換えるためです。

## 方法：

```
PowerShell $text = "こんにちは、世界！"
$text -replace "こんにちは", "Hello" 
```

このコマンドを実行すると、出力は「Hello、世界！」となります。つまり、「こんにちは」の部分が「Hello」に置き換わりました。

```
PowerShell $phrase = "私はカフェラテを頼みたいです。何がありますか？"
$phrase -replace "私は", "I would like" 
```

このコマンドでは、「私は」が「I would like」に置き換わり、出力は「I would likeカフェラテを頼みたいです。何がありますか？」となります。

## 深く掘り下げる：

検索と置換は、プログラミング言語やテキストエディタで一般的に使用される機能です。この機能は、1984年にPerlで最初に導入され、今日では多くの言語やツールで使用されています。代替としては、SedやAwkなどのツールを使用することもできます。実装の詳細については、正規表現や置換アルゴリズムなどの知識が必要になります。

## 関連リンク：

- [Microsoft公式ドキュメント](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
- [正規表現についてのチュートリアル](https://www.regular-expressions.info/powershell.html)
- [Sedの使用方法](https://www.gnu.org/software/sed/manual/sed.html)
- [Awkの使用方法](https://www.gnu.org/software/gawk/manual/gawk.html)