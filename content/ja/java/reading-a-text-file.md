---
title:                "Java: テキストファイルの読み込み"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

ソフトウェア開発者であれば、テキストファイルを読み込むことは日常的な作業です。テキストファイルには、ユーザーが入力したデータや、プログラムが生成したデータなど、多くの情報が格納されています。そのため、テキストファイルを読み込むことは、プログラムの実行に必要不可欠なスキルです。この記事では、Javaでテキストファイルを読み込む方法について解説します。

## 方法

テキストファイルを読み込むには、主に2つの方法があります。

### バッファリーダーを使用する方法

まずは、バッファリーダーを使用した方法を紹介します。バッファリーダーは、文字ストリームをバッファリングすることで、ファイルや配列などのデータを高速に読み込むことができます。

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class FileReaderExample {
    public static void main(String[] args) {
        try {
            // ファイルのパスを指定してバッファリーダーを作成
            BufferedReader reader = new BufferedReader(new FileReader("sample_data.txt"));
            
            // 1行ずつ読み込んで出力する
            String line = reader.readLine();
            while (line != null) {
                System.out.println(line);
                line = reader.readLine();
            }
            
            // リーダーを閉じる
            reader.close();
        } catch (IOException e) {
            System.out.println("ファイルの読み込みに失敗しました！");
            e.printStackTrace();
        }
    }
}
```

上記のコードでは、まずファイルパスを指定してバッファリーダーを作成し、1行ずつ読み込んで出力しています。最後に、リーダーを閉じることを忘れないようにしましょう。

### スキャナーを使用する方法

次に、スキャナーを使用した方法を紹介します。スキャナーは、基本的なデータ型や文字列など、さまざまな種類のデータを読み込むことができる便利なクラスです。

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class FileReaderExample {
    public static void main(String[] args) {
        try {
            // ファイルのパスを指定してスキャナーを作成
            Scanner scanner = new Scanner(new File("sample_data.txt"));
            
            // 1行ずつ読み込んで出力する
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                System.out.println(line);
            }
            
            // スキャナーを閉じる
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("ファイルが見つかりませんでした！");
            e.printStackTrace();
        }
    }
}
```

上記のコードでは、ファイルパスを指定してスキャナーを作成し、1行ずつ読み込んで出力しています。最後に、スキャナーを閉じることを忘れないようにしましょう。

## 詳細を深める

テキストファイルを読み込む際には、フォーマットや文字コードなどの扱いに気をつける必要があります。また、ファイルが大きい場合には、効率的に読み込む方法を考える必要があります。さらに、エラー処理も重要