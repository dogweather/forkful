---
title:    "C#: ディレクトリが存在するか確認する"
keywords: ["C#"]
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することは、プログラマーにとって重要です。作業中にプログラムが期待通りに動作するかを確認するために、ディレクトリが存在するかどうかを確認する必要があります。また、ファイルを保存する前にディレクトリの存在をチェックすることで、エラーを防ぐことができます。

## 使い方

```C#
// ディレクトリが存在するかどうかを確認するプログラム例

using System;
using System.IO;

class MainClass {
  static void Main() {

    // ディレクトリのパスを指定
    string directoryPath = @"C:\Users\Username\Desktop\SampleDirectory";

    // ディレクトリが存在するかどうかを確認
    if(Directory.Exists(directoryPath)) {
      Console.WriteLine("ディレクトリが存在します。");
    }
    else {
      Console.WriteLine("ディレクトリが存在しません。");
    }
  }
}
```

### 出力結果

```
ディレクトリが存在します。
```

## 深堀り

ディレクトリが存在するかどうかを確認するには、`System.IO` 名前空間にある`Directory.Exists()`メソッドを使用します。このメソッドは、指定したパスが存在するかどうかをブール値で返します。このメソッドを使用する前に、パスの書式が正しいことを確認することも重要です。

また、ディレクトリが存在しない場合は新しく作成する`Directory.CreateDirectory()`メソッドを組み合わせることで、プログラムでディレクトリを作成することもできます。

## この記事へのリンク

もしディレクトリが存在しない場合に、新しく作成する方法についてもっと知りたい方は、[こちらの記事](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/file-system/how-to-create-a-directory-in-net)をご覧ください。

## 参考リンク

- [C#のファイルとディレクトリを操作する方法](https://www.atmarkit.co.jp/ait/articles/1804/18/news024.html)
- [C#言語でファイルやフォルダの存在を確認する方法](https://tech.medpeer.co.jp/entry/2017/09/19/145054)
- [ディレクトリクラスのメソッドとプロパティ](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.directory?view=netframework-4.8)