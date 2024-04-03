---
date: 2024-01-26 04:12:22.022515-07:00
description: "\u4F7F\u3044\u65B9\uFF1A C#\u306E\u74B0\u5883\u3067REPL\u3092\u8D77\u52D5\
  \u3059\u308B\u306B\u306F\u3001C# Interactive\u30A6\u30A3\u30F3\u30C9\u30A6\u3092\
  \u4F7F\u7528\u3059\u308B\u304B\u3001\u7AEF\u672B\u3067`dotnet-script`\u3092\u5B9F\
  \u884C\u3057\u307E\u3059\u3002\u3053\u308C\u3092\u4F7F\u7528\u3059\u308B\u969B\u306E\
  \u4E00\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.125716-06:00'
model: gpt-4-0125-preview
summary: "C#\u306E\u74B0\u5883\u3067REPL\u3092\u8D77\u52D5\u3059\u308B\u306B\u306F\
  \u3001C# Interactive\u30A6\u30A3\u30F3\u30C9\u30A6\u3092\u4F7F\u7528\u3059\u308B\
  \u304B\u3001\u7AEF\u672B\u3067`dotnet-script`\u3092\u5B9F\u884C\u3057\u307E\u3059\
  \u3002\u3053\u308C\u3092\u4F7F\u7528\u3059\u308B\u969B\u306E\u4E00\u4F8B\u3092\u4EE5\
  \u4E0B\u306B\u793A\u3057\u307E\u3059\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 使い方：
C#の環境でREPLを起動するには、C# Interactiveウィンドウを使用するか、端末で`dotnet-script`を実行します。これを使用する際の一例を以下に示します：

```csharp
> var greeting = "こんにちは、REPL!";
> Console.WriteLine(greeting);
こんにちは、REPL!
>
```

即座にフィードバックが得られます。コンパイルや実行のステップは不要です。コードを書いて、結果を見るだけです。

## 詳細な解説
REPLはLispから現代の言語に至るまで旅を続け、Pythonのような動的な言語では栄えています。C#では、Roslynが開発者にREPLをより身近なものにしました。Roslyn用の`csi`と、.NET Core用の`dotnet-script`は、堅実なオプションです。さらに詳しく：これらはコードを一括ではなく、行ごとに評価し、典型的なC#アプリとは異なる実行モデルを採用しています。これは、実行間の状態の持続と変数のスコープに影響を与えます。

Visual StudioのC# Interactiveウィンドウは、Roslynによって動力を供給されるREPLです。Intellisense、複数の参照、NuGetパッケージのサポートがあります。初期のコマンドライン実験からは、かなり進歩したステップアップです。

他の言語に関しては、Pythonは`IDLE`を、JavaScriptはNode.jsのREPLを、F#は`F# Interactive`を使用します。これらはすべて、小さなコードスニペットのテストや言語機能の理解に無価値ではない即時フィードバックループを育んでいます。

## 参照
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)
