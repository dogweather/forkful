---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何？そしてなぜ？

テキストファイルの読み取りは、ユーザーがプログラムを介して情報を受け取るプロセスです。これにより、静的な情報を簡単に共有し、複数のプログラムやユーザー間で再利用できます。

## 方法:

次のサンプルコードは、Java（バージョン16.0.2）を使ってtextファイルを読み込む基本的な方法を示しています。

```Java
import java.nio.file.*;

public class ReadFile {
    public static void main(String[] args) {
        try {
            Path fileName = Path.of("test.txt");
            String content = Files.readString(fileName);
            System.out.println("Contents of the file:");
            System.out.println(content);
        } catch (Exception e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        } 
    }
}
```
上記のコードを実行すると、次のような結果が得られます。

```Java
Contents of the file:
Hello, world!
```

## 深堀

ファイルを読み込むためのJavaAPIは、時間とともに大きく進化してきました。初期のJavaでは`BufferedReader`と`FileReader`が主に使用されていましたが、Java 7以降ではNIOAPIが導入され、`Path`と`Files`クラスを使ってファイルの読み取りが容易になりました。

テキストファイルの読み込みには他にも方法があります。例えば、 `Scanner`クラスを使用することで、ファイルを簡単に読み取り、解析することができます。

また、ファイルの大きさや内容によって最適な方法が変わる可能性があるため、注意が必要です。一部の方法は大規模なテキストファイルを読み込むのに適していますが、他の方法は小さなファイルに対してより効率的です。

## 関連リンク

- [Oracle Java Documentation](https://docs.oracle.com/en/java/)
- [Java File I/O (NIO.2) Tutorial](https://docs.oracle.com/javase/tutorial/essential/io/index.html)