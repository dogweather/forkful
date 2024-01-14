---
title:                "C#: デバッグ出力の印刷"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 為に
デバッグ出力をプリントする理由は、コーディングのプロセスにおいてバグを発見しやすくするためです。

## 方法
デバッグ出力をプリントするには、C#の内部処理の中で```Console.WriteLine```を使用します。これにより、コードの特定の箇所で変数の値やメッセージを表示することができます。例を示します。

```C#
int num = 5;
Console.WriteLine("The value of num is: " + num);
```

このコードを実行すると、「The value of num is: 5」というメッセージが出力されます。これにより、特定の変数の値を確認したり、どの行でエラーが発生しているかを特定することができます。

## ディープダイブ
デバッグ出力は、プログラムが複雑になったりバグが生じたりした場合に非常に有用です。しかし、コードに多くのデバッグ出力を入れすぎると、パフォーマンスが低下する可能性があります。そのため、デバッグ出力を含めるべき箇所を慎重に選択することが重要です。

また、デバッグ出力は開発中のみに使われるべきで、リリースする際には全て削除することが推奨されます。デバッグ出力を残したままリリースすると、プログラムの動作に影響を及ぼす可能性があります。

## 参考リンク
- [C# Console.WriteLineメソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.console.writeline?view=net-5.0)
- [デバッグ出力が有効な.Netプログラムを記録する](https://docs.microsoft.com/ja-jp/dotnet/framework/debug-trace-profile/debug-write-to-stdout)
- [Debug.WriteLine()メソッドを使用してプログラムの実行中にコンソールにログを書き込む](https://www.codeproject.com/tips/88588/debug-write-in-csharp-program-running-output-for)
- [デバッグ出力のパフォーマンスへの影響](https://stackoverflow.com/questions/7432477/how-much-does-debug-writeline-affect-performance)

## 関連記事
参考になる他の記事を見つけるために、Markdownのヘッダー「関連記事」をチェックしてみてください。