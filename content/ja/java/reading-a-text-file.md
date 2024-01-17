---
title:                "テキストファイルの読み込み"
html_title:           "Java: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何する & なぜするの？
テキストファイルを読むとは何かを説明するために2〜3文を書くと、プログラマーがなぜそれをするのかを説明する。

テキストファイルを読むとは、プログラムやデータを含むテキストで書かれたファイルから情報を読み取ることです。プログラマーは、この情報を使用してプログラムを作成したり、データを処理したりすることができます。

## 方法：
```Java
public class ReadTextFile{
  public static void main(String[] args) throws Exception{
    // ファイルを開く
    File file = new File("sample.txt");
    // ファイルを読み込むためのリーダーを作成
    BufferedReader reader = new BufferedReader(new FileReader(file));

    String line;
    // ファイルから1行ずつ読み込んで出力
    while((line = reader.readLine()) != null){
      System.out.println(line);
    }

    // リーダーを閉じる
    reader.close();
  }
}
```
### サンプル出力：
```
Hello, world!
This is a sample text file.
```

## 詳細について：
### 歴史的背景：
テキストファイルを読む方法は、最初のプログラム言語であるFORTRANが開発された時代から存在しています。当時は外部のデータストレージが限られていたため、テキストファイルがプログラムやデータの主な手段でした。

### 代替手段：
テキストファイルの代わりに、より高度なデータ形式を使用することもできます。例えば、データベースやXMLなどです。

### 実装の詳細：
テキストファイルを読む方法は、言語やプログラミング環境に依存します。この記事ではJavaを使用していますが、他の言語でも同様の機能を持つ場合があります。

## 関連情報：
- [Javaでファイルから読み込む方法 - Java入門](https://java-beginner.com/java-file-load/)
- [ファイルからデータを読み込むには？ - W3Schools 日本語版](https://www.w3schools.com/java/java_files_read.asp)
- [Java入門テキストファイルからの読み込み - Qiita](https://qiita.com/kazamidori/items/b12dae74920def9c21af)