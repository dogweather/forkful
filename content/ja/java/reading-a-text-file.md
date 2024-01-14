---
title:    "Java: テキストファイルを読む"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ
テキストファイルを読むことの利点についてご紹介します。読者は、この記事を読むことでテキストファイルの読み方を学ぶことができます。

## こうやって
テキストファイルを読むには、Javaの標準パッケージであるjava.ioパッケージを使用します。まず、ファイルを読み込むためのFileReaderオブジェクトを作成し、それをBufferedReaderに渡します。次に、BufferedReaderからreadLine()メソッドを使用してテキストを読み込み、それを出力することでテキストファイルを読むことができます。

```Java
// ファイルを開く
File file = new File("sample.txt");

// FileReaderを作成し、BufferedReaderに渡す
BufferedReader reader = new BufferedReader(new FileReader(file));

// readLine()メソッドを使用してテキストファイルを1行ずつ読み込む
String line = reader.readLine();
while (line != null) {
    // 読み込んだテキストを出力する
    System.out.println(line);
    // 次の行を読み込む
    line = reader.readLine();
}

// ファイルを閉じる
reader.close();
```

### 出力例
このコードを実行すると、sample.txtに記載されたテキストが1行ずつコンソールに出力されます。

```
こんにちは！
私はテキストファイルです。
どうぞよろしくお願いします！
```

## 詳しく学ぶ
この記事では、テキストファイルを読むための基本的な方法をご紹介しましたが、実際にはさまざまな方法があります。例えば、Scannerクラスを使用することで、ファイル内の特定のキーワードを検索することもできます。また、ファイルが大きすぎて一度に全て読み込めない場合は、BufferedReaderではなく、BufferedInputStreamを使用することもできます。

さらに、テキストファイルのエンコーディングによっては読み込み方が異なる場合もあります。そのため、Javaでテキストファイルを読む際は、エンコーディングにも注意する必要があります。

## それ以外も見てね
- [Javaでファイルを読み書きする方法 (TechAcademy)](https://www.techacademy.jp/magazine/8233)
- [BufferedReaderクラスの使い方 (Java Tutorials)](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [Java テキストファイルの読み書き (Javaドキュメント)](https://docs.oracle.com/javase/jp/17/docs/api/java.base/java/io/FileReader.html)