---
title:                "テキストファイルを読む"
html_title:           "Java: テキストファイルを読む"
simple_title:         "テキストファイルを読む"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ
テキストファイルを読み込むことに興味を持った人にとって、この記事はどのような役割を果たすのでしょうか？答えはシンプルです。テキストファイルの読み込みは、プログラミングの基本的なスキルであり、多くの実用的なシナリオで役に立つからです。

## 方法
まずは簡単な例から始めましょう。以下のコードブロックは、JavaのScannerクラスを使用してテキストファイルを読み込む方法を示しています。

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class ReadTextFile {
  public static void main(String[] args) throws FileNotFoundException {
    // ファイルのパスを指定してScannerオブジェクトを作成
    File file = new File("sample.txt");
    Scanner sc = new Scanner(file);

    // ファイルの内容を1行ずつ読み込み、出力
    while (sc.hasNextLine()) {
      String line = sc.nextLine();
      System.out.println(line);
    }

    // Scannerを閉じる
    sc.close();
  }
}
```

これは「sample.txt」という名前のファイルの内容を読み込み、それぞれの行をコンソールに出力します。コードを実行すると、以下のような結果が得られます。

```
This is a sample text file. 
It contains multiple lines of text. 
We will use Java to read and display its contents.
```

もちろん、この例は単純なものであり、実際にはより複雑なプログラムが必要になることもあります。しかし、基本的な考え方は同じです。まずはファイルを開き、Scannerを使用してデータを読み込み、必要に応じてデータの処理を行います。

## ディープダイブ
テキストファイルを読み込む際には、いくつかのポイントに注意する必要があります。まず、ファイルを開く際にはエラー処理を行う必要があります。例えば、指定したファイルが存在しない場合や読み込み権限がない場合には、プログラムがエラーを発生させます。そのため、try-catchブロックを使用してエラーをキャッチするようにしましょう。

また、テキストファイルは文字列の集合であり、実際には改行や空白といった特殊な文字も含まれています。そのため、Scannerを使用する際には適切なデータ型を選択することが重要です。例えば、一行ずつ読み込みたい場合にはStringオブジェクトを使用し、整数や浮動小数点数を読み込みたい場合にはintやdoubleなどの適切なデータ型を使用しましょう。

## さらに見る
他にも便利なテキストファイルの読み込み方法や応用例を知りたい方は、以下のリンクを参考にしてみてください。

- [Javaでテキストファイルを読み込む方法](https://java-code.jp/509)
- [Scannerクラスのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/util/Scanner.html)