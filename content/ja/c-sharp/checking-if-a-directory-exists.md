---
title:                "C#: ディレクトリが存在するかどうかをチェックする"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
ディレクトリが存在するかどうかを確認することの理由は、ファイル操作を行う際にプログラマーが誤ったディレクトリにアクセスしてしまうことを防ぐためです。

## How To
まずは、システムにアクセスしてディレクトリが存在するかどうかを確認するために、DirectoryクラスのExistsメソッドを使用します。下記のように記述します。

```C#
if (Directory.Exists(@"C:\Users\Username\Documents"))
{
    Console.WriteLine("The directory exists.");
}
else
{
    Console.WriteLine("The directory does not exist.");
}
```

出力例：
```
The directory exists.
```

もしも存在しないディレクトリを指定した場合は、以下のような出力になります。

```C#
if (Directory.Exists(@"C:\Users\Username\Downloads"))
{
    Console.WriteLine("The directory exists.");
}
else
{
    Console.WriteLine("The directory does not exist.");
}
```

出力例：
```
The directory does not exist.
```

## Deep Dive
Directory.Existsメソッドは、指定したディレクトリの存在を確認するためにシステムにアクセスします。このメソッドを使用することで、ディレクトリを作成したり削除したりする前に、事前に存在を確認することができます。また、ディレクトリが存在しない場合は例外をスローするため、エラーハンドリングの必要があります。

## See Also
- [Directory.Exists Method (System.IO)](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=netcore-3.1)
- [Directoryクラス (System.IO)](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.directory?view=netcore-3.1)