---
title:                "Java: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

# なぜテキストファイルを書くのか

テキストファイルを作成することは、プログラマーにとって非常に役に立ちます。例えば、設定ファイルやログファイルを作成したり、データを永続的に保存するために使用します。テキストファイルは、プログラムを実行するために必要な情報を保存するための便利な方法です。

## 作り方

テキストファイルをJavaで作る方法はとても簡単です。最初に、ファイルを作成するための必要なクラスをインポートします。

```Java
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
```

次に、作成したいテキストファイルの名前を指定します。

```Java
String fileName = "sample.txt";
```

そして、ファイルオブジェクトを作成し、ファイルを作成する準備をします。

```Java
File file = new File(fileName);
```

ファイルを作成するためのファイルクラスのインスタンスを作成したら、ファイルを書き込むためのファイルライターを作成します。

```Java
try {
    FileWriter writer = new FileWriter(file);
} catch (IOException e) {
    e.printStackTrace();
}
```

そして、実際にファイルに書き込みます。

```Java
try {
    writer.write("これはテキストファイルに書き込まれたテキストです。");
    writer.close();
} catch (IOException e) {
    e.printStackTrace();
}
```

これで、指定したファイル名でテキストファイルが作成され、その中には指定したテキストが書き込まれます。

## 深い掘り下げ

テキストファイルを作成する方法についてはこの記事で十分に説明しましたが、それだけではありません。テキストファイルを開く、読み込む、編集するための他の便利な方法もあります。Javaの標準ライブラリには、これらのタスクを簡単に行うための多くのクラスやメソッドが用意されています。さらに、外部ライブラリを使用することで、より高度な機能を持つテキストファイル処理が可能になります。

# 参考リンク

- [Javaでファイルを作成する - geeksforgeeks.org](https://www.geeksforgeeks.org/java-program-create-file/)
- [Javaでテキストファイルを作成する - javatpoint.com](https://www.javatpoint.com/java-filewriter-class)
- [Javaでテキストファイルを読み書きする方法 - baeldung.com](https://www.baeldung.com/java-write-to-file)
- [Apache Commons IO - commons.apache.org](https://commons.apache.org/proper/commons-io/) (高度なテキストファイル処理のための外部ライブラリ) 

# 参考資料

- [Javaでファイルを作成する方法 - tutorialspoint.com](https://www.tutorialspoint.com/java/java_files_io.htm)
- [Javaでファイルの読み書きを行う - java2novice.com](https://www.java2novice.com/java-file-io-operations/read-write-file/)
- [Javaでファイルを読み書きする方法 - journaldev.com](https://www.journaldev.com/246/java-file-read-write)
- [Java.io.File - docs.oracle.com](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) (Javaの公式ドキュメント) 

# これを参考にする

- Javaでテキスト