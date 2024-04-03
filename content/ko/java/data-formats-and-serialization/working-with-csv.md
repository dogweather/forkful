---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:28.005793-07:00
description: "\uBC29\uBC95: #."
lastmod: '2024-03-13T22:44:55.082632-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

## 방법:


### 표준 Java 라이브러리를 사용하여 CSV 파일 읽기
Java는 표준 라이브러리에서 CSV를 직접 지원하지 않지만, `java.io` 클래스를 사용하여 쉽게 CSV 파일을 읽을 수 있습니다.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // CSV 파일의 경로 지정
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // 쉼표가 구분자로 가정
                // 데이터 처리
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

### 표준 Java 라이브러리를 사용하여 CSV 파일 쓰기
CSV 파일에 데이터를 쓰려면 `FileWriter` 및 `BufferedWriter`와 같은 `java.io` 클래스를 사용할 수 있습니다.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // 출력 CSV 파일 경로 지정

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // 쉼표가 구분자로 가정
            }
            sb.deleteCharAt(sb.length() - 1); // 마지막 쉼표 제거
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### 타사 라이브러리 사용하기: Apache Commons CSV
Apache Commons CSV는 Java에서 CSV 파일을 다루기 위한 인기 있는 라이브러리입니다. 이는 CSV 파일을 읽고 쓰는 작업을 대폭 단순화합니다.

프로젝트에 종속성 추가하기:

Maven용:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- 최신 버전 확인 -->
</dependency>
```

#### CSV 파일 읽기:
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
                // 열 인덱스로 값에 접근
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

#### CSV 파일 쓰기:
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
            csvPrinter.printRecord((Object[]) data); // 여기서는 Object[]로 형변환이 필요합니다
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV는 필드 내의 인용 부호와 쉼표와 같은 복잡성을 자동으로 처리하여, Java에서 CSV 조작에 있어 강력한 선택이 됩니다.
