---
title:                "文字パターンに合致する文字の削除"
html_title:           "C#: 文字パターンに合致する文字の削除"
simple_title:         "文字パターンに合致する文字の削除"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

プログラマーにとって、コードの読みやすさや保守性は非常に重要です。不要な文字を削除することで、コードの見通しがよくなり、エラーの原因も少なくなります。そのため、パターンにマッチする文字を削除する方法について学ぶことは、プログラマーとしてのスキルアップにつながります。

## 使い方

文字列から特定のパターンにマッチする文字を削除する方法を示します。まずは、削除したい文字列とパターンを選択します。次に、以下のようにコードを記述します。

``` C#
string input = "今日はとても暑いです"; //削除したい文字列
string pattern = "とても"; //パターン
string output = Regex.Replace(input, pattern, ""); //パターンにマッチする文字を削除
Console.WriteLine(output); //出力結果：今日は暑いです
```

ここでは、C#の`Regex`クラスを使用して、パターンにマッチする文字を削除しています。また、パターンは正規表現で指定することもできます。例えば、`[\u3040-\u309F]`というパターンは、ひらがなを表します。

## 深堀り

この方法では、削除したい文字列の中に複数回同じパターンが含まれている場合、最初にマッチした文字列しか削除されません。全てのマッチした文字列を削除するには、`Regex.Replace`メソッドの3つ目の引数として、`RegexOptions`列挙体の`RegexOptions.IgnoreCase | RegexOptions.Multiline`を指定する必要があります。

また、正規表現では、さまざまなパターンマッチングの方法があります。例えば、`*`や`+`などのワイルドカードを使用して、複数の文字列を一度に削除することもできます。さらに、マッチした文字列を特定の文字列に置換することもできます。

## 参考リンク

- [C#のRegexクラスドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.text.regularexpressions.regex)
- [正規表現入門](https://qiita.com/jesus_isao/items/622f25d2f5d1c23e8c27)
- [今すぐ使えるかんたん正規表現ユーザーズガイド](https://www.amazon.co.jp/dp/B008H04HKW/ref=dp-kindle-redirect?_encoding=UTF8&btkr=1)