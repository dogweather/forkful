---
title:                "一時ファイルの作成"
html_title:           "C#: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ
一時的なファイルを作成する理由は、プログラム実行中に一時的に必要なデータを保存するためです。例えば、大きなファイルを作成する際に、途中経過を保存するために一時的なファイルを使用することができます。

## 作り方
まず、System.IOパッケージをインポートして、外部ファイルを操作する準備をします。

```C#
using System.IO;
```

次に、`Path`クラスを使用して、一時的なファイルの場所と名前を指定します。

```C#
string path = Path.GetTempFileName();
```

上のコードでは、デフォルトの場所に、ランダムなファイル名の一時的なファイルが作成されます。ファイル名を指定するには、次のようにします。

```C#
string path = Path.GetTempFileName("file_name");
```

また、任意のディレクトリに、一時的なファイルを作成することもできます。

```C#
string dir = @"C:\Temp";
string path = Path.Combine(dir, "file_name.tmp");
```

上の例では、CドライブのTempフォルダに、file_name.tmpという名前の一時的なファイルが作成されます。

さらに、`File`クラスを使用して、一時的なファイルにデータを書き込んだり、読み取ったりすることができます。

```C#
// データの書き込み
string data = "Hello world!";
File.WriteAllText(path, data);

// データの読み取り
string readData = File.ReadAllText(path);
Console.WriteLine(readData);
// Output: Hello world!
```

## ディープダイブ
一時的なファイルは、プログラムの最適化やデータの一時的な保存に役立つことができます。また、一時的なファイルはプログラム実行中に自動的に削除されるため、ディスクの空き容量を自動的に確保することができます。

## 参考リンク
- [Path.GetTempFileName メソッド (System.IO)](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
- [File クラス (System.IO)](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file?view=net-5.0)