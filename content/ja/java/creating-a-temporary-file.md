---
title:    "Java: 一時ファイルの作成"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成する理由は、一時的にデータを保存するためです。例えば、一時ファイルを作成することで、プログラム実行中にデータを保存し、後で再利用することができます。

## 作り方

一時ファイルを作成するには、`File`クラスと`FileWriter`クラスを使用します。以下のコードブロックに、作成と書き込みの例を示します。

```Java
// 一時ファイルを作成
File tempFile = File.createTempFile("temp", ".txt");

// テキストを書き込むためのFileWriterを作成
FileWriter writer = new FileWriter(tempFile);

// テキストを書き込む
writer.write("一時ファイルに保存するデータ");

// ファイルを閉じる
writer.close();
```

一時ファイルは、デフォルトではシステムの一時フォルダに作られます。ファイル名は、接頭語「temp」とランダムに生成される数字の組み合わせ、および拡張子「.txt」で構成されます。また、一時ファイルはプログラム終了時に自動的に削除されます。

作成した一時ファイルは、通常のファイルと同じように扱うことができます。例えば、ファイルを読み込んだり、別のファイルにコピーしたりできます。

## ディープダイブ

一時ファイルを作成する際には、いくつかのオプションがあります。例えば、一時フォルダの場所を指定することもできます。以下のコードブロックに、一時ファイルを作成する際にオプションを指定する例を示します。

```Java
// 一時フォルダを指定
File tempDir = new File("C:\\temp");

// tempDir内に一時ファイルを作成
File tempFile = File.createTempFile("temp", ".txt", tempDir);
```

また、一時ファイルを空のファイルとして作成することもできます。その場合は、`createNewFile()`メソッドを使用します。

そして、一時ファイルを削除するには、`delete()`メソッドを使用します。ただし、プログラム終了時に自動的に削除されるため、明示的に削除する必要はありません。

## 参考リンク

- [Java Fileクラスドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/io/File.html)
- [Java FileWriterクラスドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/io/FileWriter.html)
- [Java TempFileクラスドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Java Fileクラスの使い方 〜ファイルやディレクトリの操作まとめ〜](https://tech.pjin.jp/blog/2017/03/24/file-class/)
- [Javaの一時ファイル操作について理解する](https://qiita.com/Ryoma0411/items/affa95b11eb1e1854f5b)