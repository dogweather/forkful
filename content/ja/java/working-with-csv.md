---
title:                "「csvとの作業」"
html_title:           "Java: 「csvとの作業」"
simple_title:         "「csvとの作業」"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-csv.md"
---

{{< edit_this_page >}}

## 何をするのか？: 

CSVとは、データを表形式で保存するフォーマットのことです。プログラマーは、データベースやスプレッドシートに保存されたデータを処理する際によく使用します。CSVはカンマで区切られた値を含むテキストファイルです。

プログラマーがCSVを使用する理由は、データをより簡単に扱えるからです。CSVは汎用的なフォーマットであり、多くのプログラミング言語でサポートされているため、データを統一的に扱うことができます。また、データベースやスプレッドシートといった他のファイル形式よりもコンパクトであり、ストレージや転送のコストを抑えることができます。

## 方法：

JavaでCSVファイルを読み込むには、まずCSVReaderクラスをインスタンス化します。次に、CSVファイルのパスを指定してreaderオブジェクトを生成し、そのオブジェクトのreadNext()メソッドを使用してCSVファイルの内容を読み込みます。以下は、CSVファイル内のデータを一行ずつ出力する例です。

```Java
CSVReader reader = new CSVReader(new FileReader("data.csv"));
String[] line;
while ((line = reader.readNext()) != null) {
    // カンマで区切られた値を表示する
    for(String value: line) {
        System.out.println(value);
    }
}
```

上記のコードを実行すると、data.csvの内容が以下のように表示されます。

```
apple, banana, orange
123, 456, 789
```

## 詳細を見る：

CSVは1972年に発明されました。当初は、大型コンピューターのテキストファイルを扱うために使用されていましたが、今ではメインフレーム以外のコンピューターでも使用されるようになりました。CSVには様々なバリエーションがあり、カンマ以外の区切り文字を使用することもできます。ただし、データの整列性を保持するためには、すべてのデータが同じ数の列に分けられている必要があります。

他のデータフォーマットとしては、JSONやXMLがあります。これらはテキスト形式でデータを保存することができますが、CSVよりも複雑な構造を持っているため、データの取得や処理がより複雑になる場合があります。

Javaでは、OpenCSVやApache Commons CSVといったライブラリを使用することで、CSVファイルの読み書きを行うことができます。これらのライブラリを使用することで、CSVファイルに対する処理を簡略化することができます。

## 関連リンク：

- [OpenCSV library](http://opencsv.sourceforge.net/)
- [Apache Commons CSV library](https://commons.apache.org/proper/commons-csv/)
- [CSV Wikipediaページ](https://ja.wikipedia.org/wiki/Comma-Separated_Values)