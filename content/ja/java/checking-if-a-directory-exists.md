---
title:                "Java: ディレクトリの存在をチェックする"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜ
ディレクトリが存在するかどうかを確認することの重要性は、プログラミングの世界では常に意識されています。ディレクトリが存在しない場合、そのディレクトリを基準とした操作を試みるとエラーが発生するため、プログラムの正しい動作を保証するためにディレクトリの存在を確認することは不可欠です。

## 方法
ディレクトリが存在するかどうかを確認するには、JavaのFileクラスを使用します。以下のコードを参考にしてください。

```Java
import java.io.File;

public class CheckDirectory {

    public static void main(String[] args) {

        // 存在するディレクトリのパスを指定
        String path = "/Users/username/Documents";

        // Fileオブジェクトを作成
        File directory = new File(path);

        // ディレクトリが存在するかどうかを確認
        if (directory.exists()) {
            System.out.println("指定されたディレクトリは存在します。");
        } else {
            System.out.println("指定されたディレクトリは存在しません。");
        }
    }
}
```

上記のコードを実行すると、指定されたパスのディレクトリが存在するかどうかがコンソールに表示されます。

## 詳細
ディレクトリが存在するかどうかを確認する方法には、他にも多くのバリエーションが存在します。例えば、Filesクラスを使用してディレクトリの存在を確認できます。また、ファイル操作に便利なライブラリであるApache Commons IOを使用することもできます。

しかし、どの方法を使用しても基本的な考え方は同じです。まずはディレクトリのパスを指定し、それをFileオブジェクトに変換してから、そのオブジェクトのexists()メソッドを使用してディレクトリの存在を確認するという流れです。

# また参照
- [Java Fileクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java Filesクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/)