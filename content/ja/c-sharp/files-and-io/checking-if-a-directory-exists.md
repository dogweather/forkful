---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:13.724020-07:00
description: "C#\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\u30D5\u30A1\u30A4\
  \u30EB\u30B7\u30B9\u30C6\u30E0\u5185\u306E\u6307\u5B9A\u3055\u308C\u305F\u30D1\u30B9\
  \u3067\u30D5\u30A9\u30EB\u30C0\u30FC\u306E\u5B58\u5728\u3092\u691C\u8A3C\u3059\u308B\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u5B58\u5728\u3057\u306A\u3044\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\
  \u304B\u3089\u306E\u8AAD\u307F\u53D6\u308A\u3084\u66F8\u304D\u8FBC\u307F\u3092\u8A66\
  \u307F\u308B\u3068\u3044\u3063\u305F\u30A8\u30E9\u30FC\u3092\u907F\u3051\u308B\u305F\
  \u3081\u3001\u307E\u305F\u30D5\u30A1\u30A4\u30EB\u3084\u30C7\u30A3\u30EC\u30AF\u30C8\
  \u30EA\u306E\u64CD\u4F5C\u3092\u30B9\u30E0\u30FC\u30BA\u306B\u3059\u308B\u305F\u3081\
  \u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.144166-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\
  \u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\
  \u30B7\u30B9\u30C6\u30E0\u5185\u306E\u6307\u5B9A\u3055\u308C\u305F\u30D1\u30B9\u3067\
  \u30D5\u30A9\u30EB\u30C0\u30FC\u306E\u5B58\u5728\u3092\u691C\u8A3C\u3059\u308B\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u5B58\u5728\u3057\u306A\u3044\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304B\
  \u3089\u306E\u8AAD\u307F\u53D6\u308A\u3084\u66F8\u304D\u8FBC\u307F\u3092\u8A66\u307F\
  \u308B\u3068\u3044\u3063\u305F\u30A8\u30E9\u30FC\u3092\u907F\u3051\u308B\u305F\u3081\
  \u3001\u307E\u305F\u30D5\u30A1\u30A4\u30EB\u3084\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\
  \u306E\u64CD\u4F5C\u3092\u30B9\u30E0\u30FC\u30BA\u306B\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
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
