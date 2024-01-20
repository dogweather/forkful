---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から特定のパターンに一致する文字を削除するとは、一連の文字を見つけてそれらを取り除くプロセスのことです。これはデータ正規化や形式の取り扱いを容易にするために行われます。

## 使い方：

```C#
// 文字列から指定したパターンを削除
string source = "これはテストです。";
string pattern = "テスト";
string result = source.Replace(pattern, "");
Console.WriteLine(result); // "これはです。"が出力されます。

// パターンを正規表現で指定
string source2 = "これは123テストです。";
string pattern2 = @"\d+"; //数字を表す。
string result2 = Regex.Replace(source2, pattern2, "");
Console.WriteLine(result2); // "これはテストです。"が出力されます。
```

## 深掘り：

文字列のパターン削除は、コンピュータプログラムがまだ初期段階にあった頃から存在します。Windows環境では、'.NET Framework'が登場する以前にも適用されていました。

制御文字や特殊文字を削除するための異なる方法があります。`String.Replace`や`Regex.Replace`の他に、LINQを使っても可能です。しかしながら、これらのメソッドはそれぞれ異なる状況とパフォーマンスに対応しているので、使用する際には計画的に選ぶことが必要です。

これらの機能は、フレームワークに組み込まれているため、一般的には、.NETランタイムの一部であるCommon Language Runtime (CLR)によって内部的に処理されます。

## 参考に：

- [公式の文字列操作ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/strings/)
- [公式の正規表現ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expressions)
- [LINQについて](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/concepts/linq/)