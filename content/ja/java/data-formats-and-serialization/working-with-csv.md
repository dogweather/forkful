---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:32.283940-07:00
description: "\u65B9\u6CD5: Java\u306F\u305D\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3067CSV\u3092\u7D44\u307F\u8FBC\u307F\u3067\u30B5\u30DD\u30FC\u30C8\u3057\
  \u3066\u3044\u307E\u305B\u3093\u304C\u3001`java.io`\u30AF\u30E9\u30B9\u3092\u4F7F\
  \u7528\u3057\u3066\u7C21\u5358\u306BCSV\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\
  \u8FBC\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.868941-06:00'
model: gpt-4-0125-preview
summary: "Java\u306F\u305D\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3067CSV\u3092\
  \u7D44\u307F\u8FBC\u307F\u3067\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\
  \u3093\u304C\u3001`java.io`\u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u3066\u7C21\
  \u5358\u306BCSV\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\u3080\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002"
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法:


### 標準Javaライブラリを使用してCSVファイルを読み込む
Javaはその標準ライブラリでCSVを組み込みでサポートしていませんが、`java.io`クラスを使用して簡単にCSVファイルを読み込むことができます。

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // CSVファイルへのパスを指定
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // コンマが区切り文字と仮定
                // データの処理
                for (String value : values) {
                    System.out.print(value + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### 標準Javaライブラリを使用してCSVファイルに書き込む
CSVファイルにデータを書き込むためには、`FileWriter`や`BufferedWriter`などの`java.io`クラスを使用できます。

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // 出力CSVファイルパスを指定

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // コンマが区切り文字と仮定
            }
            sb.deleteCharAt(sb.length() - 1); // 最後のコンマを削除
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### サードパーティライブラリを使用する：Apache Commons CSV
Apache Commons CSVは、JavaでCSVファイルを扱うための人気のあるライブラリです。これは、CSVファイルの読み書きを大幅に簡素化します。

プロジェクトに依存関係を追加する：

Mavenの場合：

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- 最新バージョンを確認 -->
</dependency>
```

#### CSVファイルを読み込む：
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.Reader;
import java.io.FileReader;
import java.io.IOException;

public class ApacheReadCSVExample {
    public static void main(String[] args) {
        String csvFile = "data.csv";
        try (Reader reader = new FileReader(csvFile);
             CSVParser csvParser = new CSVParser(reader, CSVFormat.DEFAULT)) {
            for (CSVRecord csvRecord : csvParser) {
                // 列のインデックスによって値にアクセス
                String columnOne = csvRecord.get(0);
                String columnTwo = csvRecord.get(1);
                System.out.println(columnOne + " " + columnTwo);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

#### CSVファイルに書き込む：
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"First Name", "Last Name", "Age", "City"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // ここでは(Object[])へのキャストが必要
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSVは、フィールド内の引用符やコンマなどの複雑さを自動的に扱い、JavaでのCSV操作において堅牢な選択肢となります。
