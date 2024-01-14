---
title:    "C#: パターンにマッチする文字を削除する"
keywords: ["C#"]
---

{{< edit_this_page >}}

## なぜ

なぜ誰かがパターンにマッチする文字を削除することに取り組むのか、その理由について説明します。

## 方法

以下の「```C# ... ```」コードブロック内のコーディング例とサンプル出力を参考に、文字を削除する方法を紹介します。

```C#
using System;

public class Program
{
    public static void Main()
    {
        // 文字列を定義
        string str = "aabbccdd1234567890";

        // パターンにマッチする文字を削除
        string newStr = System.Text.RegularExpressions.Regex.Replace(str, @"[a-z]", "");

        // 結果を出力
        Console.WriteLine("元の文字列: " + str);
        Console.WriteLine("パターンにマッチする文字を削除した後: " + newStr);
    }
}

// 出力結果
// 元の文字列: aabbccdd1234567890
// パターンにマッチする文字を削除した後: 1234567890
```

## ディープダイブ

パターンにマッチする文字を削除する方法について、詳しく説明します。削除する際に使用される `System.Text.RegularExpressions` ライブラリや、正規表現を使用してパターンを指定する方法など、より詳細な情報を紹介します。

## この記事を読んだ人にお勧めの記事

- [C#で正規表現を使用して文字を置換する方法](https://lang-ship.com/blog/work/csharp/regex-replace)
- [正規表現を使った文字列のパターンマッチング入門](https://knowledge.sakura.ad.jp/20443/)
- [正規表現の書き方とパターン一覧](https://qiita.com/jnchito/items/893c887fbf19e17d3ff9)