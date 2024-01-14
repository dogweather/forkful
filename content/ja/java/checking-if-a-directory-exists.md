---
title:                "Java: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ
ファイルやディレクトリが存在するかどうかを確認することは、プログラマーにとって非常に重要なタスクです。実行中のプログラムで既存のファイルやディレクトリにアクセスする必要がある場合、それらが存在しないことを前提としていると、予期しないエラーが発生してプログラムが停止してしまう可能性があります。そのため、事前にファイルやディレクトリが存在するかどうかを確認し、処理を行う前に適切な対処をすることが重要です。

## 作り方
Javaでは、Fileクラスのexists()メソッドを使うことで、指定したファイルやディレクトリが存在するかどうかを確認することができます。下記のように、if文でこのメソッドの戻り値を条件として使用することで、ファイルやディレクトリが存在する場合と存在しない場合でそれぞれ異なる処理を行うことができます。

```Java
File file = new File("C:/Documents/example.txt");
if (file.exists()) {
  System.out.println("ファイルが存在します。");
} else {
  System.out.println("ファイルが存在しません。");
}
```

出力結果は以下のようになります。

```
ファイルが存在します。
```

## ディープダイブ
より詳細な情報を追加する場合、FileクラスのisFile()メソッドやisDirectory()メソッドを使用することで、ファイルかどうかやディレクトリかどうかの判定が可能です。また、前述のexists()メソッドは、実際にファイルやディレクトリが存在するかどうかを確認するために、ファイルシステムにアクセスするため、パフォーマンスの観点からは注意が必要です。そのため、isFile()メソッドやisDirectory()メソッドを使用するのがより効率的な方法と言えます。

## See Also
- [Java File.exists() Method](https://www.w3schools.com/java/java_files.asp)
- [Java File.isFile() Method](https://www.guru99.com/java-file-exists.html)
- [Java File.isDirectory() Method](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#isDirectory--)