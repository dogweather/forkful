---
title:                "テキストファイルの読み込み"
html_title:           "C#: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何をするのか？ 
テキストファイルとは、テキスト情報を含むファイルのことです。プログラマーはこのテキストファイルを読み込むことができます。読み込んだテキストファイルの中身を変数に格納し、プログラムの中で使用することができます。

## 方法： 
プログラミングコードの例と出力例を示します。 

```C#
using System; 

class Program 
{ 
  static void Main(string[] args) 
  { 
    // テキストファイルの読み込み 
    string[] lines = System.IO.File.ReadAllLines(@"C:\sample.txt"); 

    // 読み込んだテキストファイルの中身をコンソールに出力 
    foreach (string line in lines) 
    { 
      Console.WriteLine(line); 
    } 
  } 
}
```

出力例：
```
Hello World!
こんにちは世界！
12345
```

## 深く掘り下げる 
テキストファイルの読み込みは、古くからあるプログラミングの基本的な機能です。テキストファイルの代わりにデータベースを使用することもできますが、小さなデータや一時的な情報を保存する場合は、テキストファイルの使用が便利です。C#では、```System.IO.File``` クラスを使用してテキストファイルを読み込むことができます。

## 関連リンク
- [C#入門 | テキストファイルの読み込み](https://www.sejuku.net/blog/61614)
- [C# ドキュメント | File クラス](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file?view=netcore-3.1)