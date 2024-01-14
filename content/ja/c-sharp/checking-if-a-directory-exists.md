---
title:    "C#: ディレクトリが存在するかをチェックする"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why (なぜ):

ディレクトリが存在するかどうかを確認することは、プログラミングにおいて重要なタスクの一つです。特定のディレクトリが存在するかどうかを確認すると、プログラムの実行中に不意のエラーを防ぐことができるため、コードの安定性を確保することができます。

## How To (方法):

ディレクトリが存在するかどうかを確認するには、まずSystem.IO名前空間を使用する必要があります。次に、DirectoryクラスのExistsメソッドを使って、指定したディレクトリが存在するかどうかを確認します。

```
C# using System.IO;
C# 
C# string directoryPath = "C:/Users/User/Documents/";
C# //ディレクトリが存在するかどうかを確認する
C# if (Directory.Exists(directoryPath)) {
C#     Console.WriteLine("ディレクトリが存在します。");
C# }
C# else {
C#     Console.WriteLine("ディレクトリが存在しません。");
C# }
```
上記の例では、ディレクトリが存在する場合は"ディレクトリが存在します。"が、存在しない場合は"ディレクトリが存在しません。"が出力されます。

## Deep Dive (詳細):

Directory.Existsメソッドは、指定したパスがディレクトリであるかどうかを確認し、存在する場合はtrueを、存在しない場合はfalseを返します。また、このメソッドでは、パスが空の文字列またはnullである場合もfalseを返します。さらに、指定したパスがネットワークドライブ上の共有ディレクトリである場合、存在しない場合もしくはアクセスできない場合は、Exceptionがスローされます。

See Also (関連リンク):
- [Directory.Existsメソッドのドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
- [C#でディレクトリを作成する方法](https://www.sejuku.net/blog/8383)
- [ディレクトリやフォルダを移動する方法](https://www.j-n.co.jp/column/p0249.html)