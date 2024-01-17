---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Java: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何＆なぜ？

ディレクトリの存在を確認することは、プログラマーがあるディレクトリが存在するかどうかを確認することです。プログラマーがディレクトリの存在を確認する理由は、プログラムの実行中に必要なファイルやディレクトリが存在しない場合にエラーを防ぐことができるからです。

## 方法：

```Java 
String directoryPath = "C:/Users/TestFolder";
File directory = new File(directoryPath);

if (directory.exists()) {
    System.out.println("The directory exists.");
} else {
    System.out.println("The directory does not exist.");
}
```
```
出力：
The directory exists.
```

## 詳細情報：

(1) ディレクトリの存在を確認する機能は、Java 1.2から追加されました。それ以前のバージョンでは、ファイルをチェックすることで代用する必要がありました。

(2) ディレクトリの存在を確認する方法として、 `exists()` メソッドの他にも `isDirectory()` メソッドを使用することもできます。このメソッドは、指定されたファイルパスがディレクトリであるかどうかを判断することができます。

(3) `exists()` メソッドは、ディレクトリだけでなく、ファイルの存在も確認することができます。また、 `isDirectory()` メソッドは、指定されたファイルパスがディレクトリではない場合にも `false` を返します。

## 関連情報：

- [Java File Class Documentation](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/File.html)
- [How to Create a Directory in Java](https://www.baeldung.com/java-create-directory)
- [Introduction to the File System API in Java](https://www.dummies.com/programming/java/introduction-to-the-file-system-api-in-java/)