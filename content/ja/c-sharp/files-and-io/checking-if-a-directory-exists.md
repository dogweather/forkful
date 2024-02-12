---
title:                "ディレクトリが存在するかどうかの確認"
aliases:
- ja/c-sharp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:13.724020-07:00
model:                 gpt-4-0125-preview
simple_title:         "ディレクトリが存在するかどうかの確認"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

C#でディレクトリが存在するかを確認することは、ファイルシステム内の指定されたパスでフォルダーの存在を検証することを意味します。プログラマーは、存在しないディレクトリからの読み取りや書き込みを試みるといったエラーを避けるため、またファイルやディレクトリの操作をスムーズにするためにこれを行います。

## 方法:

### System.IO を使用する

C#は`System.IO`名前空間を提供しており、ここにはディレクトリの存在を`Exists`メソッドを通じて直接確認するための`Directory`クラスが含まれています。

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // ディレクトリが存在するか確認
        bool directoryExists = Directory.Exists(directoryPath);

        // 結果を表示
        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**サンプル出力:**

```
Directory exists: False
```

`C:\ExampleDirectory`のパスにディレクトリが存在する場合、出力は`True`になります。

### 単体テストのための System.IO.Abstractions の使用

ファイルシステムとやり取りするコードを単体テスト可能にする際、`System.IO.Abstractions`パッケージは人気のある選択肢です。これを使用すると、テスト内でファイルシステムの操作を抽象化してモックできます。以下はこのアプローチを使用してディレクトリの存在を確認する方法です：

まず、パッケージをインストールしてください：

```
Install-Package System.IO.Abstractions
```

その後、`IFileSystem`をクラスに注入して使用し、ディレクトリが存在するかを確認できます。これにより、単体テストが容易になります。

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**サンプル出力:**

```
Directory exists: False
```

このアプローチは、アプリケーションロジックを直接的なファイルシステムアクセスから分離し、コードをよりモジュール化され、テスト可能で、保守しやすくします。
