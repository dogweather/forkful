---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
CSV 파일은 데이터를 쉽게 저장하고 교환하는 텍스트 형식입니다. 프로그래머들은 데이터 분석, 데이터 이동, 또는 대규모 정보를 처리할 때 CSV를 자주 사용합니다.

## How to: (어떻게 하나요?)
Java에서 CSV 파일을 읽고 쓰는 예제입니다.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class CSVExample {
    public static void main(String[] args) {
        String filePath = "example.csv";

        // CSV 읽기
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                // 값을 활용하세요.
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        // CSV 쓰기
        try (FileWriter writer = new FileWriter(filePath)) {
            String[] data = {"Java", "CSV", "Example"};
            writer.append(String.join(",", data));
            writer.append("\n");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

## Deep Dive (깊이 있는 정보)
CSV(Comma-Separated Values) 형식은 1970년대부터 사용되었습니다. XML이나 JSON과 같은 대체 형식이 있지만, CSV는 단순함과 가독성 때문에 여전히 인기가 있습니다. Java에서는 `java.io` 패키지로 기본적인 CSV 처리를 할 수 있고, Apache Commons CSV나 OpenCSV와 같은 라이브러리로 더 복잡한 기능을 구현할 수 있습니다.

## See Also (더 참고할 정보)
- Java의 `java.io` 패키지 문서: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/package-summary.html
- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- OpenCSV: http://opencsv.sourceforge.net/
