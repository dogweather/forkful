---
title:                "ディレクトリの存在を確認する"
html_title:           "Java: ディレクトリの存在を確認する"
simple_title:         "ディレクトリの存在を確認する"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することに何の意味があるのでしょうか？その理由を簡潔に説明します。

ディレクトリが存在するかどうかを確認することは、ファイルシステムの中で特定のファイルが存在するかどうかを確認する重要なステップです。例えば、ユーザーにとって重要なデータが格納されているディレクトリが存在しない場合、アプリケーションは動作しない可能性があります。ディレクトリが存在するかどうかを事前に確認することで、問題を早めに発見し、対応することができます。

## 方法

ディレクトリが存在するかどうかを確認するには、`java.io.File`クラスの`exists()`メソッドを使用します。以下の例をご覧ください。

```java
import java.io.File;

public class DirectoryExistExample {

    public static void main(String[] args) {

        // 存在しないディレクトリのパスを指定
        File directory = new File("C:/Users/Example/userData");

        if (directory.exists()) {
            // ディレクトリが存在する場合の処理
            System.out.println("ディレクトリが存在します。");
        } else {
            // ディレクトリが存在しない場合の処理
            System.out.println("ディレクトリは存在しません。");
        }
    }
}
```

上記のコードを実行すると、出力結果は以下のようになります。

```java
ディレクトリは存在しません。
```

## ディープダイブ

`exists()`メソッドが`true`を返した場合、ファイルまたはディレクトリが実際に存在することが保証されません。なぜなら、その間にファイルやディレクトリが削除される可能性があるからです。また、`exists()`メソッドはシンボリックリンクをフォローするため、チェックするパスがリンクである場合も`true`を返します。

さらに、ファイルがコンピュータ上で実際に存在するかどうかを確認するという点で、`exists()`メソッドは信頼性のある方法ではありません。そのため、`exists()`メソッドを使用する前には、その結果をもとにさらなる処理を行うことをお勧めします。

## 参考リンク

- [Javaドキュメントー`File`クラスの`exists()`メソッド](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)
- [コード例ーJavaでディレクトリが存在するかどうかを確認する方法](https://www.codejava.net/java-se/file-io/how-to-check-if-directory-exists-in-java)
- [スタックオーバーフローーJavaで`exists()`メソッドを使用する際の注意点](https://stackoverflow.com/questions/27644361/java-jfile-exists-giving-false-value-for-directory-even-if-directory-exists)