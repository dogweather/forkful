---
title:                "ディレクトリが存在するかどうかをチェックする"
html_title:           "C#: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することが重要な場合があります。例えば、ファイルを読み込む前にディレクトリが存在するかどうかを確認することで、プログラムの安定性や動作を保証することができます。

## 方法

最も簡単な方法は、`System.IO.Directory`クラスの`Exists()`メソッドを使用することです。このメソッドはディレクトリが存在する場合には`true`、存在しない場合には`false`を返します。

```C#
if (Directory.Exists("C:\目的のディレクトリ"))
{
  Console.WriteLine("ディレクトリは存在します。");
}
else
{
  Console.WriteLine("ディレクトリは存在しません。");
}
```

また、より詳細な情報を得るためには、`DirectoryInfo`クラスを使用することができます。このクラスには、`Exists()`メソッドの他にもディレクトリの作成日時や更新日時などの情報を取得できるメソッドがあります。

```C#
DirectoryInfo directory = new DirectoryInfo("C:\目的のディレクトリ");

if (directory.Exists)
{
  Console.WriteLine("ディレクトリは存在します。");
  Console.WriteLine("作成日時：" + directory.CreationTime);
  Console.WriteLine("更新日時：" + directory.LastWriteTime);
}
else
{
  Console.WriteLine("ディレクトリは存在しません。");
}
```

## 深堀り

ディレクトリが存在するかどうかを確認する方法は、`File`クラスの`Exists()`メソッドと同様です。しかし、ファイルとは異なり、ディレクトリには`Hidden`属性があることに注意する必要があります。`Hidden`属性が設定されている場合、`File`クラスの`Exists()`メソッドでは正しい結果が返されない場合があります。そのため、ディレクトリの存在を確認する場合には`Directory`クラスを使用するようにしましょう。

## さらに読む

- [C# Directory.Exists メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.directory.exists)
- [C# DirectoryInfo クラス](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.directoryinfo)
- [C# ファイル操作入門](https://programming.dpinfo.co.jp/csharp/how-to-file-io/)