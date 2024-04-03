---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:13.724020-07:00
description: "\u65B9\u6CD5: #."
lastmod: '2024-03-13T22:44:42.144166-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

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
