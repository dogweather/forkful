---
title:                "一時ファイルを作成する"
html_title:           "Java: 一時ファイルを作成する"
simple_title:         "一時ファイルを作成する"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

作業中に一時ファイルを作成することは、プログラマーにとって便利な方法です。一時ファイルを使用することで、様々な処理を実行した後、作業結果を保存することができます。また、一時ファイルは使い終わった後に自動的に削除されるため、不要なファイルを手動で削除する必要がありません。

## 作り方

一時ファイルを作成するには、まず`java.io.File`クラスを使用します。以下のコードは、一時ファイルを作成し、テキストファイルに書き込む例です。

```Java
import java.io.*;
import java.util.*;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // 一時ファイルを作成するディレクトリを指定
            File tempDir = new File("C:/Temp");
            // 一時ファイルを作成
            File tempFile = File.createTempFile("temp", ".txt", tempDir);
            // テキストを書き込むためのPrintWriterを作成
            PrintWriter writer = new PrintWriter(tempFile);
            // テキストを書き込む
            writer.println("これは一時ファイルです。");
            // ファイルをクローズ
            writer.close();
            // ファイル名を出力
            System.out.println("作成した一時ファイルの名前は " + tempFile.getName() + " です。");
            // 作成したファイルを削除
            tempFile.delete();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

上記のコードを実行すると、指定したディレクトリに`temp`で始まる一時ファイルが生成されます。また、ファイル名を出力する部分では、作成した一時ファイルの名前が表示されます。

## 深堀り

一時ファイルはJVMが終了すると自動的に削除されますが、使用中にアプリケーションがクラッシュした場合は削除されません。そのため、一時ファイルを使用する際には、ファイルが使用中であるかどうかを確認し、必要に応じて削除するようにすることが重要です。

また、一時ファイルを作成する際には、一時ファイルを保存するディレクトリを指定する必要があります。一般的には、プログラムが実行されているディレクトリや、OSが提供する一時ファイルのディレクトリ（例：Windowsの場合は`C:/Users/<ユーザー名>/AppData/Local/Temp`）が使用されます。

## 関連リンク

- [Javaの公式ドキュメンテーション](https://docs.oracle.com/javase/jp/8/docs/api/)
- [Java Fileクラスの解説](https://www.tohoho-web.com/java/file.htm)