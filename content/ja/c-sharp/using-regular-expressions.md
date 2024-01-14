---
title:    "C#: 正規表現を使う"
keywords: ["C#"]
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由は、文字のパターンマッチングやバリデーションを行うためです。これにより、文字列に対するより柔軟な操作が可能になり、プログラミングの効率が向上します。

## 使い方

```C#
// 文字列がメールアドレス形式であるかをチェックする
string email = "example@example.com";
if (Regex.IsMatch(email, @"^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$"))
{
    Console.WriteLine("有効なメールアドレスです。");
} 
else
{
    Console.WriteLine("無効なメールアドレスです。");
}

// 文字列から数字のみを抽出する
string sentence = "今日は20日です。明日は21日です。";
MatchCollection matches = Regex.Matches(sentence, @"\d+");
foreach (Match match in matches)
{
    Console.WriteLine(match.Value);
}
```

**出力**

```
有効なメールアドレスです。
20
21
```

## 深堀り

正規表現は非常に強力なツールであり、複雑な文字列操作を行うことができます。しかし、パターンの書き方やメタ文字の使い方など、正規表現の文法を学ぶことは少し時間がかかるかもしれません。しかし、コーディングの効率を向上させるために正規表現をマスターすることは非常に価値のあることです。

##  関連リンク

- [C#で正規表現を使って文字列操作する方法](https://www.slideshare.net/gihyodocojp/c-1)
- [正規表現の基礎知識](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [正規表現のテストツール](https://regex101.com/)