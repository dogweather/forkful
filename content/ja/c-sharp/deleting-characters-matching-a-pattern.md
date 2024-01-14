---
title:    "C#: パターンにマッチする文字の削除"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

あなたはプログラミングにおいて、特定のパターンに一致する文字を削除する必要が生じることがあります。その理由は多岐にわたります。例えばエラーメッセージから不要な文字を削除するため、またはデータベースから特定のフィールドを除外するためなどです。

## 方法

文字列を処理する際に、特定のパターンに一致する文字を削除する方法を学ぶことで、より効率的にコードを書くことができます。下記のコード例を参考にしてください。

```C#
// 文字列から特定の文字を削除するメソッド
public string RemoveCharacters(string input, string pattern)
{
    string output = Regex.Replace(input, pattern, "");
    return output;
}

// 使用例：エラーメッセージからカッコと中身を削除する
string errorMessage = "Invalid input (12345). Please try again.";
string pattern = @"\s\(.*?\)";

string result = RemoveCharacters(errorMessage, pattern);
Console.WriteLine(result); // 出力結果：Invalid input. Please try again.
```

## 深堀り

文字を削除する際にパターンに注目することで、より複雑な処理を行うことができます。例えば、文字列内の特定のパターンに一致する文字を取得したり、削除したりすることができます。また、正規表現を使用することでより柔軟なパターンマッチングが可能になります。

## 参考資料

- Microsoft: [正規表現を使用して文字列内の特定のパターンを検索する](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/finding-patterns-in-strings-using-regular-expressions)
- C# グローバルリファレンス: [Regex.Replace メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.text.regularexpressions.regex.replace)