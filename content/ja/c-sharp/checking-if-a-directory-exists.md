---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "C#: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？
ディレクトリが存在するかどうかを確認するとは、特定のパスがディレクトリとして存在するかどうかをコードでチェックすることです。これはプログラムがデータを読み書きする前に必要なディレクトリが存在することを保証し、意図しないエラーを防ぐために行われます。

## 実践方法
次に示すのは、指定したパスのディレクトリが存在するかどうかを確認するC#コードの例です。

```C#
using System.IO;
   
public class Program
{
    public static void Main()
    {
        string dirPath = @"C:\samplepath";

        if (Directory.Exists(dirPath))
        {
            System.Console.WriteLine("Directory exists.");
        }
        else
        {
            System.Console.WriteLine("Directory does not exist.");
        }
    }
}
```
このコードを実行すると、以下のような出力結果が得られます。

```C#
Directory exists.
```
もしくは

```C#
Directory does not exist.
```

## ディープダイブ
Directory.Existsメソッドは.NET Framework自体の歴史と同様に古く、初期から存在しています。そのリーズンには、ファイルやディレクトリの存在確認が、一般的なプログラミングタスクであるためです。

代替方法としてFile.Existsメソッドがありますが、これは指定されたパスがファイルとして存在するかどうかを確認します。

Directory.ExistsとFile.Existsの両方のメソッドは、指定されたパスへのアクセス許可がない場合はfalseを返し、それ以外の場合は指定されたパスが存在するかどうかを返します。

## 参照
以下に関連リンクを掲載します。
1. [Microsoft公式ドキュメント: Directory.Exists Method](https://docs.microsoft.com/dotnet/api/system.io.directory.exists)
2. [Microsoft公式ドキュメント: File.Exists Method](https://docs.microsoft.com/dotnet/api/system.io.file.exists)
3. [Stack Overflow: How to check if a directory exists in C#](https://stackoverflow.com/questions/1395205/better-way-to-check-if-a-path-is-a-file-or-a-directory)