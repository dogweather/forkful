---
title:                "CSV 파일과 함께 작업하기"
html_title:           "Kotlin: CSV 파일과 함께 작업하기"
simple_title:         "CSV 파일과 함께 작업하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## CSV 작업이란?

CSV(Comma Separated Values)파일은 컴마(,)로 구분된 데이터를 의미합니다. 이러한 포맷은 모든 스프레드시트 프로그램에서 지원되므로 데이터의 교류가 기존 응용 프로그램에서 적은 노력으로 가능합니다. 프로그래머들은 CSV 작업을 이용하여 데이터를 저장, 분석 또는 처리하며 주로 데이터베이스 저장나 문서화 작업에 많이 사용됩니다.

## 어떻게 하나요?

CSV 작업을 위해서는 기본적으로 데이터를 읽고 쓰는 기능이 필요합니다. Kotlin에서는 표준 라이브러리에서 제공하는 ```FileReader```와 ```CSVParser```를 이용하여 간단하게 CSV 파일을 읽고 데이터를 처리할 수 있습니다. 아래 예제는 CSV 파일을 읽어 각 행의 컬럼 값을 출력하는 코드입니다.

```Kotlin
import java.io.FileReader
import org.apache.commons.csv.CSVParser

val fileReader = FileReader("data.csv")
val csvParser = CSVParser(fileReader)
val records = csvParser.getRecords()
for (record in records) {
    println(record.get(0) + "," + record.get(1) + "," + record.get(2))
}
```

출력 결과는 다음과 같습니다.

```
Name,Age,City
John,25,New York
Lisa,30,Boston
Mark,28,Chicago
```

## 깊게 파헤쳐보기

CSV는 1972년에 IBM에서 개발된 포맷으로, 당시에는 기록 장치에서 데이터를 전송하기 위해 사용되었습니다. 현재의 CSV는 RFC 4180에 기술되어 있으며, 가장 널리 사용되는 용도는 스프레드시트 프로그램과 데이터베이스 간 데이터 교환입니다.

CSV 작업을 위해서는 Kotlin 외에도 다른 언어에서도 지원되는 다양한 라이브러리가 있습니다. Apache Commons CSV 라이브러리 외에도 Super CSV, OpenCSV 등이 있으며, 각 라이브러리마다 다양한 기능을 제공합니다.

CSV 작업을 하면서 가장 중요한 것은 데이터가 올바르게 구분될 수 있도록 쓰기 규칙을 잘 지켜야 합니다. 특히 따옴표(""), 콤마(,), 개행문자 등의 특수기호를 적절히 사용하여 데이터가 올바르게 파싱되도록 해야 합니다.

## 관련 자료

- RFC 4180: https://tools.ietf.org/html/rfc4180
- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- Super CSV: http://www.supercsv.org/
- OpenCSV: http://opencsv.sourceforge.net/