---
title:                "Java: テキストファイルの作成"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ書く？

テキストファイルを作成することは、プログラミングにおいて非常に重要です。テキストファイルを使用することで、データを読み書きすることができ、プログラムの実行を制御することができます。

## 作り方

テキストファイルを作成するには、まずファイルをオープンして文字を書き込み、最後にファイルを閉じる必要があります。以下はJavaでのサンプルコードです。

```Java
import java.io.FileWriter;
import java.io.IOException;

public class WriteToFile {
  public static void main(String[] args) {
    try {
      // ファイルをオープン
      FileWriter fileWriter = new FileWriter("sample.txt");

      // 文字を書き込む
      fileWriter.write("こんにちは、世界！");

      // ファイルを閉じる
      fileWriter.close();
    } catch (IOException e) {
      System.out.println("エラーが発生しました。");
      e.printStackTrace();
    }
  }
}
```

上記のコードを実行すると、"sample.txt"というファイルが作成され、その中に"こんにちは、世界！"という文字が書き込まれます。

## より深く知る

テキストファイルを作成する際には、エラー処理をしっかりと行うことが重要です。また、ファイル形式によっては特定の文字コードを指定する必要がある場合があります。詳細はJavaの公式ドキュメントを参照してください。

## 詳細はこちらを参考にしてください

[Javaの公式ドキュメント](https://docs.oracle.com/javase/jp/)

[テキストファイルの作成方法](https://www.javadrive.jp/start/file/index1.html)

[文字コードの指定について](https://docs.oracle.com/javase/jp/8/docs/api/java/io/OutputStreamWriter.html)