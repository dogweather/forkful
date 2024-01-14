---
title:    "Java: ディレクトリが存在するかどうかを確認する"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜチェックをするのか

プログラミングをする中で、時には特定のディレクトリが存在するかどうかを知りたい場合があります。ファイルへのアクセスやファイルの作成を行う前に、ディレクトリが存在するかどうかをチェックすることは非常に重要です。そこで今回は、Javaの中でディレクトリが存在するかどうかをチェックする方法についてご紹介します。

## チェックの方法

Javaでディレクトリが存在するかどうかを確認するには、「File」クラスの「exists()」メソッドを使用します。これは、指定したファイルまたはディレクトリが存在する場合にはtrueを返し、存在しない場合にはfalseを返します。

```Java
import java.io.File;

public class CheckDirectory {

    public static void main(String[] args) {
        // チェックするディレクトリのパスを指定
        String directoryPath = "C:\\Users\\Username\\Documents\\SampleDirectory";
        
        // Fileクラスのオブジェクトを生成
        File directory = new File(directoryPath);
        
        // ディレクトリが存在するかどうかをチェック
        if (directory.exists()) {
            System.out.println("指定されたディレクトリは存在します。");
        } else {
            System.out.println("指定されたディレクトリは存在しません。");
        }
    }
}

// 出力結果:
// 指定されたディレクトリは存在します。
```

上記の例では、まず「File」クラスのオブジェクトを生成し、そのオブジェクトの「exists()」メソッドを使ってディレクトリが存在するかどうかを判定しています。もし、ディレクトリが存在する場合には、「指定されたディレクトリは存在します。」というメッセージを表示し、存在しない場合には、「指定されたディレクトリは存在しません。」というメッセージを表示しています。

## 深堀り

上記の例では、単純にディレクトリが存在するかどうかをチェックするだけでしたが、実際には存在しない場合に新しくディレクトリを作成するなど、より柔軟に処理を行うことができます。また、「File」クラスを使用する代わりに、「Files」クラスを使用することでより高度な操作が可能になります。

## 関連情報

- [Javaファイル操作入門](https://techacademy.jp/magazine/19640)
- [Javaでファイルを作成、書き込み、読み取り、コピー、削除する方法を学ぶ](https://www.codeflow.site/ja/article/java-file)
- [Java Filesクラスの使い方](https://qiita.com/h2ts0101/items/78e7c0723448a35aad8e)