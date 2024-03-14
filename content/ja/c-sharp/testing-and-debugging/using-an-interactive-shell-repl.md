---
date: 2024-01-26 04:12:22.022515-07:00
description: "REPL\uFF08Read-Eval-Print Loop\uFF09\u3092\u4F7F\u7528\u3059\u308B\u3068\
  \u3001C#\u30B3\u30FC\u30C9\u3092\u5165\u529B\u3057\u3001\u5BFE\u8A71\u7684\u306B\
  \u5B9F\u884C\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30D5\u30EB\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\
  \u306E\u8A2D\u5B9A\u306E\u30AA\u30FC\u30D0\u30FC\u30D8\u30C3\u30C9\u306A\u3057\u3067\
  \u3001C#\u306E\u30AF\u30A4\u30C3\u30AF\u306A\u5B9F\u9A13\u3001\u30C7\u30D0\u30C3\
  \u30B0\u3001\u307E\u305F\u306F\u5B66\u7FD2\u3092\u884C\u3046\u305F\u3081\u306B\u3053\
  \u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.125716-06:00'
model: gpt-4-0125-preview
summary: "REPL\uFF08Read-Eval-Print Loop\uFF09\u3092\u4F7F\u7528\u3059\u308B\u3068\
  \u3001C#\u30B3\u30FC\u30C9\u3092\u5165\u529B\u3057\u3001\u5BFE\u8A71\u7684\u306B\
  \u5B9F\u884C\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30D5\u30EB\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\
  \u306E\u8A2D\u5B9A\u306E\u30AA\u30FC\u30D0\u30FC\u30D8\u30C3\u30C9\u306A\u3057\u3067\
  \u3001C#\u306E\u30AF\u30A4\u30C3\u30AF\u306A\u5B9F\u9A13\u3001\u30C7\u30D0\u30C3\
  \u30B0\u3001\u307E\u305F\u306F\u5B66\u7FD2\u3092\u884C\u3046\u305F\u3081\u306B\u3053\
  \u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
REPL（Read-Eval-Print Loop）を使用すると、C#コードを入力し、対話的に実行することができます。プログラマーは、フルプロジェクトの設定のオーバーヘッドなしで、C#のクイックな実験、デバッグ、または学習を行うためにこれを使用します。

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
