---
title:    "C#: 「文字列を小文字に変換する」"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換することに人々が取り組むのかについて説明します。

## 方法
文字列を小文字に変換する方法についてのコーディング例とサンプル出力を示します。

```C#
string name = "JAPANESE";
string lowerName = name.ToLower();
Console.WriteLine(lowerName);
```
出力：
```
japanese
```

## 深堀り
文字列を小文字に変換する方法は、文字列の値を変更せずに新しい文字列を作成することで実現されます。これにより、オリジナルの文字列が保持されるため、必要に応じて元の大文字の文字列を使用することができます。

また、小文字に変換される文字は、現在のカルチャや文字セットに基づいて決定されるため、言語や地域によって異なる結果が得られる場合があります。

## あわせて見る
**[See Also (参考)]**
- [String.ToLower メソッド (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.tolower)
- [文字列を小文字に変換する方法 (C# シャーププログラミング)](https://csharpsharp.com/articles/c-sharp-string-lowercase/)