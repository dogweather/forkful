---
title:    "C#: 作成する一時ファイル"
keywords: ["C#"]
---

{{< edit_this_page >}}

## なぜ

プログラミングで一時ファイルを作成するメリットは何でしょうか？一時ファイルを作成することで、一時的なデータや処理結果を保持し、プログラムのパフォーマンスを向上させることができます。

## 作り方

一時ファイルを作成するには、.NET Frameworkのクラスライブラリである `System.IO` 名前空間の `Path.GetTempFileName()` メソッドを使用します。

以下は、C#で一時ファイルを作成し、ファイルパスを出力するコード例です。

```C#
string tempFile = Path.GetTempFileName();
Console.WriteLine("一時ファイルが作成されました：" + tempFile);
```

実行結果:

```
一時ファイルが作成されました：C:\Users\username\AppData\Local\Temp\tmp1234.tmp
```

## 詳細を深堀り

`Path.GetTempFileName()` メソッドを呼び出すと、一時ファイルが作成され、ファイルパスが返されます。このファイルパスは、一時的なファイル名を持つ空のファイルとなります。

`Path.GetTempFileName()` メソッドは、ユニークなファイル名を生成するために、一時ディレクトリにランダムな数字を付け加えます。この方法により、複数のアプリケーションで同じファイル名を使うことを防ぎます。

一時ファイルは、プログラムが終了すると削除されます。また、`Path.GetTempFileName()` メソッドは、既に存在するファイルは置き換えないことを保証しています。

## この記事を読んだら次は

一時ファイルを作成する他の方法や、一時ファイルを活用する方法については以下のリンクを参考にしてください。

- [Microsoft Docs - `Path.GetTempFileName()` メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.path.gettempfilename?view=netframework-4.8)
- [C# Corner - "Working with Temporary Files in C#"](https://www.c-sharpcorner.com/article/working-with-temporary-files-in-c-sharp/)