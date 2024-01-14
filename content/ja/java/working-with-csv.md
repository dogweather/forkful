---
title:                "Java: 「csvとの作業」"
simple_title:         "「csvとの作業」"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを扱うのか？

CSVは、データベースやExcelなどでよく使用されるファイル形式です。規則性のあるテキストファイルであり、データを簡単に取り扱うことができるため、プログラミングにおいても重要な役割を果たしています。CSVとはどのようなファイル形式なのか、どのように扱えばよいのかを学びましょう。

## 手順

CSVファイルの読み込みや書き込みには、Javaプログラミング言語における標準のクラスである`FileReader`と`FileWriter`を使用します。まずは、これらのオブジェクトを作成し、対象のCSVファイルを指定します。

```Java
FileReader csvFileReader = new FileReader("sample.csv");
FileWriter csvFileWriter = new FileWriter("output.csv");
```

次に、CSVファイルからデータを読み込む際には、`BufferedReader`クラスを使用します。読み込んだデータは、配列やリストなどのデータ構造に格納することができます。以下は、CSVファイルから1行ずつデータを読み込み、配列に格納する例です。

```Java
BufferedReader csvReader = new BufferedReader(csvFileReader);
String line;

while ((line = csvReader.readLine()) != null) {
    String[] data = line.split(",");
    // dataを配列に格納する処理
}
```

また、CSVファイルにデータを書き込む際には、`BufferedWriter`クラスを使用します。以下は、配列に格納されたデータをCSVファイルに書き込む例です。

```Java
BufferedWriter csvWriter = new BufferedWriter(csvFileWriter);
String[] data = {"John", "Doe", "30"};
csvWriter.write(String.join(",", data));
csvWriter.newLine();
// 書き込み処理の後、必ずclose()でファイルを閉じること
```

以上のように、CSVファイルの読み込みや書き込みには、標準のJavaクラスを使用することができます。

## CSVの深層へ

CSVファイルには、列や行のヘッダーを含めることができます。ヘッダーを含むCSVファイルからデータを読み込む際には、`CSVReader`クラスを使用します。以下は、ヘッダーを含むCSVファイルからデータを読み込み、ヘッダー名を基準にデータを取得する例です。

```Java
CSVReader reader = new CSVReader(csvReader);
String[] headers = reader.readNext();

while ((data = reader.readNext()) != null) {
    // headers配列のインデックスと同じ順番でデータを取得し、使用する
}
```

また、CSVファイルをExcelやGoogle Sheetsなどの表形式のファイルに変換することも可能です。これには、Apache POIなどのライブラリを使用することができます。CSVファイルを扱う際には、これらのライブラリも併せて学ぶことをお勧めします。

## 参考リンク

- [Oracle Java ドキュメント - CSVファイルの読み込み/書き込み](https://docs.oracle.com/javase/tutorial/essential/io/csv.html)
- [Apache POI](https://poi.apache.org/)