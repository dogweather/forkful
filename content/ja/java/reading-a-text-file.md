---
title:                "Java: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ
多くのプログラミング言語では、テキストファイルを読み込むことは非常に一般的です。テキストファイルには、アプリケーションで使用するデータや設定情報など、様々な重要な情報が含まれることがあります。そのため、Javaを学ぶ上でテキストファイルの読み込みは必須のスキルとなります。この記事では、Javaでテキストファイルを読み込む方法を紹介します。

## 使い方
Javaでは、テキストファイルを読み込むためには`java.io.FileReader`クラスを使用します。下記の例では、`FileReader`クラスを使用して"input.txt"という名前のテキストファイルを読み込み、内容をコンソールに出力しています。

```Java
import java.io.*;

public class ReadTextFile {
    public static void main(String[] args) {
        try {
            FileReader reader = new FileReader("input.txt");
            int character;
            
            while((character = reader.read()) != -1) {
                System.out.print((char) character);
            }
            
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

上記のコードを実行すると、"input.txt"の内容が1文字ずつコンソールに表示されます。もしもファイルが見つからない場合や読み込みでエラーが発生した場合、例外処理でエラーをキャッチして表示するようにしています。

## 詳細について
Javaでは、テキストファイルを読み込む方法として`java.io.FileReader`以外にも`BufferedReader`や`Scanner`クラスなども利用することができます。また、読み込んだテキストファイルの内容を加工したり、別のファイルに書き込んだりすることも可能です。

さらに、テキストファイルの読み込み方やエラー処理など、より詳細な情報を知りたい場合はJavaの公式ドキュメントを参考にしてみてください。

## この記事を読んでもっと学べること
- [Javaの公式ドキュメント](https://docs.oracle.com/javase/jp/)
- [Javaでファイルを読み書きする方法](https://qiita.com/nokok/items/912b6ac499ac51fe26d9)
- [Javaプログラミング: テキストファイルの読み込み・出力](https://www.javadrive.jp/start/file/index1.html)

＃＃＃ 関連リンク

- [Javaプログラミングの基本: ファイルの読み書き](https://www.javadrive.jp/basic/exception/index6.html)
- [Javaでテキストファイルを読み書きする方法 (ハウツー記事)](https://techacademy.jp/magazine/17127)
- [Javaでのファイル操作について理解する](https://www.sejuku.net/blog/19723)