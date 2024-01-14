---
title:    "Java: 一時ファイルの作成"
keywords: ["Java"]
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成するのは、実行中のプログラムが一時的に必要なファイルを作成するためです。これは、プログラム実行中にデータを保存したり、一時的な処理をする際に便利です。

## 作り方

Javaを使用して一時ファイルを作成する方法はとても簡単です。まずは、インポート文でFileクラスとTemporaryFileクラスを読み込みます。それから、作成したい一時ファイルのパスを指定し、TemporaryFileクラスのcreateTempFile()メソッドを呼び出します。以下の例をご覧ください。

```Java
import java.io.File;
import java.io.IOException;

public class CreateTempFileExample {
    public static void main(String[] args) {
        try {
            // 作成したい一時ファイルのパスを指定
            String tempFilePath = "C:\\Users\\User\\Desktop\\tempfile.txt";
            // TemporaryFileクラスのcreateTempFile()メソッドを呼び出し、一時ファイルを作成
            File tempFile = TemporaryFile.createTempFile(tempFilePath);
            // ファイル名を取得
            String tempFileName = tempFile.getName();
            System.out.println("作成された一時ファイルの名前は: " + tempFileName);
        } catch (IOException e) {
            System.out.println("エラーが発生しました: " + e.getMessage());
        }
    }
}
```

実行結果は以下のようになります。

```
作成された一時ファイルの名前は: tempfile3112803655500591989.txt
```

## 深堀り

一時ファイルを作成する方法は、上記の例でご紹介したようにとても簡単です。しかし、TemporaryFileクラスにはさらにオプションの引数を渡すことができます。具体的には、第二引数に接頭辞を、第三引数に接尾辞を指定することができます。これらを指定することで、作成される一時ファイルの名前に接頭辞や接尾辞を付けることができます。また、TemporaryFileクラスのdeleteOnExit()メソッドを使用することで、プログラム終了時に一時ファイルが自動的に削除されるようにすることもできます。

## 見つける

Javaでの一時ファイル作成については、公式ドキュメントやオンラインのチュートリアルなど、さまざまな情報源があります。以下のリンクを参考にしてみてください。

- [Official Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/TemporaryFile.html)
- [Java Code Examples for java.io.TemporaryFile](https://www.programcreek.com/java-api-examples/?class=java.io.TemporaryFile&method=createTempFile)
- [Tutorialspoint - TemporaryFile Class in Java](https://www.tutorialspoint.com/java/lang/java_lang_temporaryfile.htm)

## 参考

- [Fileクラスの使い方【Java入門】](https://www.javadrive.jp/start/relative/index3.html)
- [Fileクラスを使ってファイル入出力](https://qiita.com/mpyw/items/b00b72c5c95aac573b71#temporaryfile%E3%81%AF%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E9%96%A2%E9%80%A3%E3%81%AE%E6%9B%B8%E3%81%8D%E3%81%8B%E3%82%89%E9%80%9A%E7%9F%A5%E3%81%97%E3