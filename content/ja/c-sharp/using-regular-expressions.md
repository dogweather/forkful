---
title:                "C#: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ正規表現を使用するのか

正規表現は、文字列をパターンやパターンマッチングルールに基づいて効率的に検索、置換、抽出することができる強力なツールです。また、文字列やファイルの扱い方がより柔軟になります。今回のブログポストでは、C#で正規表現を使用する方法をご紹介します。

## 正規表現の使い方

正規表現を使うには、System.Text.RegularExpressions名前空間を使用する必要があります。Regexクラスの静的メソッドを使用して、パターンに基づいて文字列を処理することができます。以下の例をご覧ください。

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main(string[] args)
    {
        string text = "ブログ投稿日時は、2021-05-20です。";
        string pattern = @"\d{4}-\d{2}-\d{2}";
        Match match = Regex.Match(text, pattern);
        Console.WriteLine($"日付： {match.Value}");
    }
}
```
この場合、"2021-05-20"という文字列が"日付："の後ろに出力されます。

正規表現では、検索したい文字列の一部をグループ化することもできます。上記の例では、日付の部分をグループ化することができます。また、パターンには様々な特殊文字やメタ文字を使用することもできます。詳細な情報は、Microsoftのドキュメントを参照してください。

## 正規表現の詳細

正規表現についての詳細な情報や実際のコーディングに役立つヒントをご紹介します。まず、パターンの作成にあたっては、文字列のパターンや文字の順番に注意する必要があります。また、マッチングを行う際に、グローバルオプションやイングノアケースオプションなどを使用することもできます。さらに、正規表現内でキャプチャグループを使用する方法や、置換時のコールバック関数の使用方法など、より高度な機能もあります。

もちろん、正規表現に関する知識を深めるには実践的な経験が必要です。簡単なパターンから始め、少しずつ慣れていくことをお勧めします。

## 参考リンク

- [Microsoftドキュメント - 正規表現の使用](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expressions)
- [C#での正規表現の使い方 - Qiita](https://qiita.com/bohebohechan/items/57248ba084443958ed10)
- [正規表現チュートリアル - 正規表現.info](https://www.regular-expressions.info/)
- [正規表現エディタ - RegexStorm](https://regexstorm.net/)
- [正規表現チェッカー - Regex101](https://regex101.com/)