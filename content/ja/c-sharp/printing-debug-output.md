---
title:                "デバッグ出力の表示"
html_title:           "C#: デバッグ出力の表示"
simple_title:         "デバッグ出力の表示"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何が何故？
デバッグの出力をプリントするとは何かを説明し、プログラマーがそれを行う理由を２、３文で説明します。

デバッグの出力は、プログラムを実行中に表示されるメッセージや変数の値などの情報を指します。プログラムの実行中に何が起こっているかを確認するためや、バグを見つけるためなど、プログラマーはデバッグの出力を利用します。

## 方法：
```C# 
Console.WriteLine("Hello World"); 
```
このように、Consoleクラスを使用して、デバッグの出力を印刷することができます。また、変数の値を確認するには、次のように書くこともできます。
```C#
Console.WriteLine("変数 x の値は " + x + "です。"); 
```
注意：デバッグの出力はプログラムが完成した後は不要なので、最終的なコードには削除する必要があります。

## 詳細：
デバッグの出力は非常に便利ですが、本番環境では不要な情報を表示するため、パフォーマンスにも影響します。そのため、デバッグモードとリリースモードでは別々の方法でデバッグの出力を処理することが推奨されています。

また、デバッグの出力を記録するには、ログファイルを使用したり、デバッグツールを利用することもできます。

## 他にも見てみて：
デバッグの出力にはさまざまな使い方がありますので、より詳細な情報を知りたい方は以下のリンクを参考にしてみてください。

- [C#でデバッグを行う方法](https://docs.microsoft.com/ja-jp/visualstudio/debugger/overview-of-debugging-in-visual-studio?view=vs-2019)
- [デバッグツールの使い方](https://docs.microsoft.com/ja-jp/visualstudio/debugger/choose-the-debugger-for-your-app?view=vs-2019)
- [ログファイルの記録の方法](https://docs.microsoft.com/ja-jp/dotnet/core/extensions/logging?tabs=command-line)