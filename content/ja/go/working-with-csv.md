---
title:                "「csvファイルを扱う」"
html_title:           "Go: 「csvファイルを扱う」"
simple_title:         "「csvファイルを扱う」"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/working-with-csv.md"
---

{{< edit_this_page >}}

## CSVとは何か？

CSVは「Comma-Separated Values」の略称で、データをコンマで区切って記録するファイル形式のことです。プログラマーがCSVを使用する理由は、データの入出力を簡単にするためです。また、多くのデータベースやスプレッドシートアプリケーションがCSV形式をサポートしているため、データの共有にも便利です。

## 使い方：

Go言語でCSVを扱うには、encoding/csvパッケージを使用します。最初にCSVファイルをオープンし、その後、csv.Readerを使用してデータを読み取ったり、csv.Writerを使用してデータを書き込んだりすることができます。例えば、次のようなコードでCSVファイルを読み取り、データを出力することができます。

```Go
file, err := os.Open("data.csv")
if err != nil {
    log.Fatal(err)
}
defer file.Close()

reader := csv.NewReader(file)
records, err := reader.ReadAll()
if err != nil {
    log.Fatal(err)
}

for _, row := range records {
    fmt.Println(row)
}
```

出力例:

```Go
["John" "Doe" "30"]
["Jane" "Smith" "25"]
```

## 詳細な情報：

CSV形式は、1970年代から使用されている古くからあるファイル形式です。他の代替フォーマットとしては、タブ区切りのTSVがありますが、CSVの方がより広く使用されています。まずは、パッケージドキュメントを参照することで、より詳細な実装情報を確認できます。

## 関連リンク：

- [encoding/csvパッケージドキュメント](https://golang.org/pkg/encoding/csv/)
- [CSV vs TSV: Which is better?](https://www.ablebits.com/office-addins-blog/2015/03/04/csv-vs-tsv/)
- [CSVの歴史](https://www.openbookproject.net/tutorials/getdown/html/HistoryASCII.html#SectionHistoryCSV)