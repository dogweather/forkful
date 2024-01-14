---
title:                "C#: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ
プログラミングをしているときに、たまにテンポラリーファイルを作成することがあります。テンポラリーファイルは一時的にデータを保存するために使用されますが、その目的やメリットについてはあまり知られていません。そこで今回は、なぜテンポラリーファイルを作成するのかについてご説明します。

## 作り方
テンポラリーファイルを作成するには、まず```System.IO```名前空間を使用する必要があります。以下のように、一時的にファイルを作成するためのメソッドを用意しました。

```C#
// テンポラリーファイルを作成するメソッド
public static void CreateTempFile()
{
    // ファイルの一時的な名前を作成する
    string tempFileName = Path.GetTempFileName();

    // テンポラリーファイルを開く
    using (FileStream fs = File.Open(tempFileName, FileMode.OpenOrCreate))
    {
        // ファイルに書き込む
        byte[] byteData = Encoding.UTF8.GetBytes("Hello world!");
        fs.Write(byteData, 0, byteData.Length);
    }

    // テンポラリーファイルを閉じる
    File.Delete(tempFileName);
}
```

このように、```Path.GetTempFileName()```メソッドを使用することで一時的なファイル名を取得し、```File.Open()```メソッドを使用することでファイルを開き、必要な処理を行うことができます。

また、テンポラリーファイルを削除する際は、```File.Delete()```メソッドを使用することで簡単に削除することができます。

## 深堀り
テンポラリーファイルを作成する際には、ファイルを作成するだけでなく、同時にファイルを削除する必要があります。これは、プログラムが終了する際に不要なファイルが残らないようにするためです。また、オペレーティングシステムは時間の経過とともにテンポラリーファイルを自動的に削除することがあります。

さらに、テンポラリーファイルは一時的にデータを保存するだけでなく、メモリの使用量を抑えるためにも使用されます。一時的に利用するデータは、メモリよりもファイルに保存する方が効率的であり、テンポラリーファイルはそのような用途にも利用されます。

## 他に見る
* [一時ファイルを使うときの注意点 (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/standard/io/ux-design/temporary-files)
* [C#でファイル操作を行う (TechAcademy Magazine)](https://techacademy.jp/magazine/29517)
* [C#で文字列をバイト配列として扱う (C# 公式ドキュメント)](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/arrays/single-dimensional-arrays#string-byte-arrays)