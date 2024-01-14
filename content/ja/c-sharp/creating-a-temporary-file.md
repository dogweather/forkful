---
title:                "C#: 一時ファイルの作成 (Ichi ji fairu no sakusei)"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成する理由は、データの一時的な保管や処理に便利であるためです。

## 作り方

```C#
using System;
using System.IO;

namespace CreateTempFile
{
    class Program
    {
        static void Main(string[] args)
        {
            // ランダムなファイル名を生成
            string fileName = Path.GetRandomFileName();
            // AppDataフォルダに一時ファイルを作成
            string filePath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), fileName);
            // ファイルを作成し、書き込む
            File.Create(filePath).Close();
            File.WriteAllText(filePath, "これは一時ファイルです。");
            // ファイルのパスと中身を出力
            Console.WriteLine($"一時ファイルが作成されました：{filePath}\n中身：{File.ReadAllText(filePath)}");
        }
    }
}
```

```
一時ファイルが作成されました：C:\Users\ユーザー名\AppData\Roaming\ak32c433.etw
中身：これは一時ファイルです。
```

## 深堀り

一時ファイルは、プログラムの実行中に必要なデータを一時的に保存するために使用されます。例えば、大量のデータを処理する際には、一時ファイルを作成してそこにデータを保存し、処理が終わった後に削除することでメモリを節約することができます。また、一時ファイルを使用することで、複数回繰り返し処理を行う場合でも、毎回新しいファイルが作成されるためデータが混ざることがなく、安全に処理を行うことができます。

## 参考リンク

- [C#でファイルを作成する方法](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/file-system/how-to-create-a-file)
- [C#で一時ファイルを利用する方法](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/file-system/how-to-create-a-temporary-file)