---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "C#: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ?

ディレクトリが存在するかどうかを確認するとは、ファイルシステムに特定のディレクトリが存在しているかをJavaで調べることです。これはプログラマがファイルやディレクトリを通過する前にエラーや例外を防ぐための良い習慣であり、必要です。

## どうやって:

以下にディレクトリが存在するかどうかを確認するJavaのコード例を示します。

```java
import java.nio.file.*;

public class Main {
    public static void main(String[] args) {
        // 検証したいディレクトリのパスを指定します。
        Path path = Paths.get("/Users/your_name/Documents/test_directory");

        // Files.existsメソッドを使用してディレクトリが存在するかを確認します。
        boolean directoryExists = Files.exists(path);

        // 結果を出力します。
        if(directoryExists) {
            System.out.println("The directory exists.");
        } else {
            System.out.println("The directory does not exist.");
        }
    }
}
```

このコードを動かすと、「The directory exists.」または「The directory does not exist.」という結果が得られます。

## ディープダイブ

ディレクトリの存在を確認することはJavaが誕生した1990年代から存在します。Java7で導入された`java.nio.file`パッケージのFiles.existsメソッドは極めて便利で、異なるプラットフォームでの互換性も考慮しています。

実装面では、Files.existsはバックグラウンドで`java.nio.file.FileSystems`のgetDefaultメソッドを使ってActive File Systemを取得し、そこから所望の結果を提供します。

代わりに`java.io.File`のexistsメソッドを使用することもできますが、このメソッドは古く、非推奨です。その理由は、次のような問題があるからです: (1) I/Oエラーが発生した場合、その情報を失います, (2) セキュリティマネージャが存在する場合、未確認の結果を返す可能性があります。

## 参考

それ以降の深科学的研究のために、以下のリンクを参照してください。

1. [Oracle Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#exists-java.nio.file.Path-java.nio.file.LinkOption...-)
2. [Baeldung Guide on java.nio.file API](https://www.baeldung.com/java-nio-2-file-api)
3. [StackOverflow discussion on File.exists vs Files.exists](https://stackoverflow.com/questions/3757767/pros-and-cons-java-io-file-vs-java-nio-file)