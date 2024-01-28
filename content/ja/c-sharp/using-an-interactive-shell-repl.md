---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:12:22.022515-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/using-an-interactive-shell-repl.md"
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
