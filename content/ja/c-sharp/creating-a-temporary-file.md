---
title:    "C#: 一時的なファイルの作成"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# なぜ一時ファイルを作成するのか

一時ファイルを作成するときは、プログラムの実行中に短期間使用されるファイルが必要になった場合に役立ちます。例えば、一時的なデータを保存したり、テスト用のファイルを作成する際に使用できます。

## 作り方

一時ファイルを作成するには、`Path.GetTempFileName()`メソッドを使用します。このメソッドは、一時ファイルのパスを文字列で返します。次に、そのパスを使用してファイルを作成し、書き込みや読み取りが行えます。

```C#
var tempFilePath = Path.GetTempFileName();
// 一時ファイルの作成

using (FileStream fs = File.Create(tempFilePath))
{
    //ファイルに書き込みができるようにする
}

using (StreamReader sr = File.OpenText(tempFilePath))
{
    //ファイルからデータを読み取る
}
```

作成した一時ファイルは、プログラムの実行が終了すると自動的に削除されます。

## 深堀り

一時ファイルは、プログラムの一時的なデータの保存やテスト用のファイルの作成以外にも様々な用途に使用できます。例えば、ダウンロード中の一時ファイルを作成して、ダウンロードが完了したら削除することができます。

また、一時ファイルを使用することで、プログラムのパフォーマンスを向上させることができます。プログラムが頻繁にファイルを読み書きする場合、一時ファイルを使用することでハードディスクのアクセス回数を減らすことができます。

# 参考リンク

- [C#でファイル操作をする方法](https://qiita.com/moriyaman/items/bd3b1e282e53566e5cd1)
- [一時ファイルの作成と使用方法](https://docs.microsoft.com/ja-jp/dotnet/standard/io/how-to-create-temporary-files)