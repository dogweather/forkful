---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列のインターポーレーションとは、文字列中に変数を直接埋め込むことです。これを行う理由は、コードの可読性を向上させ、エラーを減らすためです。

## どのように実装するか

次は文字列内挿（string interpolation）の基本的なC#のコード例です:

```C#
string name = "Yamada";
string greeting = $"こんにちは、{name}さん";
Console.WriteLine(greeting);
```

出力:

```
こんにちは、Yamadaさん
```

次のようなより複雑な例もあります:

```C#
int x = 10, y = 20;
Console.WriteLine($"xとyの和は{x + y}です");
```

出力:

```
xとyの和は30です
```

## 深掘り

1. **歴史的背景**: 文字列内挿はC# 6.0で導入されました。それ以前は`string.Format`や`StringBuilder`を使用していました。
2. **代替手段**: `string.Format`は内挿前の代替手段ですが、視覚的に直感的でないかもしれません。別の選択肢としては`StringBuilder`がありますが、これは主に大量の文字列操作に最適です。
3. **実装詳細**: 内挿された文字列は、C# コンパイラによって `string.Format`メソッド呼び出しに変換されます。つまり、`$"こんにちは、{name}さん"`は`string.Format("こんにちは、{0}さん", name)`に対応します。

## 関連リソース

- [Microsoft Docs: 文字列の内挿 (C# チュートリアル)](https://docs.microsoft.com/ja-jp/dotnet/csharp/tutorials/string-interpolation)