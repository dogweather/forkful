---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSVは「Comma-Separated Values（カンマ区切り値）」の略で、テキストデータを簡単に表形式で保存するためのフォーマットです。プログラマはCSVを使ってデータのインポートやエクスポートを行い、異なるシステム間でデータを簡単にやり取りするために使用します。

## How to:
以下はJavaでCSVファイルを読み込み、書き込む基本的な例です。

```Java
import java.io.*;
import java.nio.file.*;
import java.util.*;

public class CSVExample {
    public static void main(String[] args) throws IOException {
        String fileName = "example.csv";

        // CSV読み込み
        List<String[]> data = readCSV(fileName);
        for (String[] line : data) {
            System.out.println(Arrays.toString(line));
        }

        // CSV書き込み
        String[] newData = {"5", "Ender", "Wiggin"};
        writeCSV(fileName, newData);
    }

    public static List<String[]> readCSV(String fileName) throws IOException {
        List<String[]> content = new ArrayList<>();
        Path pathToFile = Paths.get(fileName);

        try (BufferedReader br = Files.newBufferedReader(pathToFile)) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] attributes = line.split(",");
                content.add(attributes);
            }
        }
        return content;
    }

    public static void writeCSV(String fileName, String[] data) throws IOException {
        Path pathToFile = Paths.get(fileName);

        try (BufferedWriter bw = Files.newBufferedWriter(pathToFile, StandardOpenOption.APPEND)) {
            bw.write(String.join(",", data));
            bw.newLine();
        }
    }
}
```

出力例:
```
[1, John, Doe]
[2, Jane, Smith]
[3, Peter, Jones]
```

## Deep Dive
CSVは1972年にIBMで開発され、テキストデータのシンプルな保存フォーマットとして普及しました。XMLやJSONといった代替フォーマットも存在しますが、CSVはその簡潔さからデータの表現において引き続き人気があります。JavaでのCSV処理は`BufferedReader`と`BufferedWriter`で行うのが基本ですが、Apache Commons CSVやOpenCSVのようなライブラリを使えばさらに簡単になります。

## See Also
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/)
- [OpenCSV](http://opencsv.sourceforge.net/)
- [RFC 4180](https://www.ietf.org/rfc/rfc4180.txt) - CSVに関する公式の仕様書
