---
title:                "コードを関数に整理する"
aliases:
- /ja/c-sharp/organizing-code-into-functions/
date:                  2024-01-26T01:09:57.638052-07:00
model:                 gpt-4-1106-preview
simple_title:         "コードを関数に整理する"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 何となく理由
コードを関数に分割することは、LEGOブロックを分類するようなものです。探しやすくなり、利用も簡単になります。これを行う理由は、繰り返しを避けるため、理解を単純化するため、そしてメンテナンスを頭痛の種にしないためです。

## 方法
複数回、あいさつを出力するコードがあったとしましょう。関数を使用しない場合は、ごちゃごちゃしていますが、関数を使用するときれいになります。

```C#
// 関数を使用せず - 繰り返しが多い
Console.WriteLine("こんにちは、エイミー！");
Console.WriteLine("こんにちは、ボブ！");
Console.WriteLine("こんにちは、チャーリー！");

// 関数を使用して - よりすっきり
void Greet(string name) {
    Console.WriteLine($"こんにちは、{name}！");
}

Greet("エイミー");
Greet("ボブ");
Greet("チャーリー");
```

出力はどちらも同じですが、2番目のバージョンの方がずっと整頓されています。

## 深掘り
昔、アセンブリ言語の時代には、GOTOを使用して異なるコードの位置に移動していましたが、これは混沌としており追跡が困難でした。関数は、ツールボックスの整理された引き出しのような、大きなレベルアップです。代替手段？もちろんあります。クラスの文脈において関数はメソッドとなりますし、ラムダやインライン関数はその場限りの短いタスクに利用できます。

実装については、小さく集中した関数が最高です。テストやデバッグが容易になります。多くの責任を持つ大きな関数は手に負えないものになる可能性があり、「スパゲッティコード」という不名誉な称号を得ることがあります。関数には一つの仕事をさせること；後で感謝するでしょう。

## 参考
関数とベストプラクティスについてもっと知りたい場合は、以下をチェックしてください：

- Robert C. Martinの「Clean Code」：関数をきれいに保つ原則。
- Martin Fowlerの「Refactoring」：既存のコードを改善する方法。
- Microsoft C#ガイド「Methods」: https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/classes-and-structs/methods
