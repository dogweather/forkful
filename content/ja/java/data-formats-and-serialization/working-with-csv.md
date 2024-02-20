---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:32.283940-07:00
description: "\u2026"
lastmod: 2024-02-19 22:05:01.137470
model: gpt-4-0125-preview
summary: "\u2026"
title: "CSV\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## 何となぜ？

CSVファイルの扱いは、コンマ区切り値（CSV）ファイルからの読み取りとデータの書き込みを含みます。これはデータ交換において人気のある形式です。それはシンプルで広くサポートされているためです。プログラマーは、データのインポート/エクスポート、データ分析、異なるシステム間の情報共有などのタスクのためにCSVファイルを操作します。

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
