---
title:                "「CSVを扱う」"
html_title:           "Java: 「CSVを扱う」"
simple_title:         "「CSVを扱う」"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜ

CSVファイルはデータを格納しやすく、共有しやすいため、Javaプログラマーにとって非常に便利なファイル形式です。この記事では、CSVファイルを効率的に扱う方法について説明します。

## 方法

CSVファイルを操作するためには、Javaの標準ライブラリである`java.io`パッケージの`FileReader`と`BufferedReader`クラスを使用します。以下の例を参考にしてください。

```Java
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

public class CSVReader {
    public static void main(String[] args) {
        try {
            // CSVファイルのパスを指定してFileReaderを作成
            FileReader fileReader = new FileReader("sample.csv");
            // FileReaderをBufferedReaderでラップ
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            // CSVファイルを1行ずつ読み込んで処理
            String line;
            while ((line = bufferedReader.readLine()) != null) {
                // 読み込んだ行をカンマで分割し、配列に格納
                String[] data = line.split(",");
                // 各データを処理する
                // 例えば、data[0]は1列目のデータ
                System.out.println(data[0]);
            }
            // リソースを解放
            bufferedReader.close();
        } catch (FileNotFoundException e) {
            System.out.println("ファイルが見つかりません。");
        } catch (IOException e) {
            System.out.println("入出力エラーが発生しました。");
        }
    }
}
```

上記のコードは、`sample.csv`という名前のCSVファイルを読み込み、各行のデータを`String`配列として取得します。詳細な処理内容は省略しますが、データを加工したりデータベースに保存したりすることができます。

## 深堀り

CSVファイルにはデータの区切り文字としてカンマが使われることが多いですが、別の文字で区切られている場合もあります。そういった場合は、`split()`メソッドの引数にカンマ以外の文字を指定することで対応できます。また、CSVファイルにはヘッダーが含まれることもありますが、ヘッダーをスキップしてデータを取得する方法もありますので、必要に応じて調べてみてください。

## 関連記事

- [JavaでCSVファイルを扱う方法](https://www.javadrive.jp/start/file/index8.html)
- [FileReaderクラスの使い方](https://java-code.jp/143)
- [BufferedReaderクラスの使い方](https://java-code.jp/144)