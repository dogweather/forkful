---
title:    "Java: テキストファイルの読み込み"
keywords: ["Java"]
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み取るのはなぜ必要なのでしょうか？テキストファイルはプログラミングにおいて非常に重要です。それを読み取ることにより、データを取得して処理することができるからです。

## 方法

テキストファイルを読み取るためには、Javaの標準ライブラリである `java.io` パッケージを使用します。まずは `FileReader` クラスをインスタンス化し、読み取りたいファイルのパスを指定します。その後、`BufferedReader` クラスを使用してファイルから1行ずつデータを読み取ります。以下のコードを参考にしてください。

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class TextFileReader {
    public static void main(String[] args) {
        try {
            // ファイルを開く
            FileReader fileReader = new FileReader("sample.txt");
            BufferedReader bufferedReader = new BufferedReader(fileReader);

            // ファイルから1行ずつ読み取り、コンソールに出力する
            String line = "";
            while ((line = bufferedReader.readLine()) != null) {
                System.out.println(line);
            }

            // ファイルを閉じる
            bufferedReader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

もし、読み取るファイルが単純な文字列の並びではなく、CSVやJSONなどの形式を持つ場合は、ファイルを読み取ってから必要な処理を行うことでデータを正しく取得することができます。

```Java
// CSVファイルからデータを取得する例
// ファイルの1行目にはカラム名が入っているとする
String header = bufferedReader.readLine();
String[] columnNames = header.split(","); // カラム名を配列に格納する

// データを格納するためのリストを作成する
List<Map<String, String>> dataList = new ArrayList<>();

// ファイルから1行ずつデータを読み取り、カラム名に対応した値をマップに格納する
String line = "";
while ((line = bufferedReader.readLine()) != null) {
    Map<String, String> data = new HashMap<>();
    String[] values = line.split(",");
    for (int i = 0; i < columnNames.length; i++) {
        data.put(columnNames[i], values[i]);
    }
    dataList.add(data);
}
```

## 深堀り

テキストファイルを読み取る方法についてさらに詳しく見ていきましょう。例えば、それぞれ違うエンコーディング方式を使用したファイルを読み取る必要がある場合、`InputStreamReader` クラスを使用することでエンコーディングを指定することができます。

また、ファイルの内容をコンソールに出力するだけでなく、データベースに格納したり、別のファイルに書き出したりすることもできます。ファイルの読み取り方や処理方法は様々ありますので、自分のプロジェクトに適した方法を見つけてください。

## See Also

- [Java.ioパッケージのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/io/package-summary.html)
- [JavaでCSVファイルを読み書きする方法](https://qiita.com/opengl-8080/items/6ab834a15f493a97bbb0)