---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:13.724020-07:00
description: "\u65B9\u6CD5: C#\u306F`System.IO`\u540D\u524D\u7A7A\u9593\u3092\u63D0\
  \u4F9B\u3057\u3066\u304A\u308A\u3001\u3053\u3053\u306B\u306F\u30C7\u30A3\u30EC\u30AF\
  \u30C8\u30EA\u306E\u5B58\u5728\u3092`Exists`\u30E1\u30BD\u30C3\u30C9\u3092\u901A\
  \u3058\u3066\u76F4\u63A5\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306E`Directory`\u30AF\
  \u30E9\u30B9\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:43.016225-06:00'
model: gpt-4-0125-preview
summary: "C#\u306F`System.IO`\u540D\u524D\u7A7A\u9593\u3092\u63D0\u4F9B\u3057\u3066\
  \u304A\u308A\u3001\u3053\u3053\u306B\u306F\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\
  \u5B58\u5728\u3092`Exists`\u30E1\u30BD\u30C3\u30C9\u3092\u901A\u3058\u3066\u76F4\
  \u63A5\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306E`Directory`\u30AF\u30E9\u30B9\u304C\
  \u542B\u307E\u308C\u3066\u3044\u307E\u3059\u3002"
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
