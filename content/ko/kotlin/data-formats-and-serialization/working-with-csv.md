---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:36.213215-07:00
description: "CSV(\uCF64\uB9C8\uB85C \uAD6C\uBD84\uB41C \uAC12) \uC791\uC5C5\uC740\
  \ \uC77C\uBC18 \uD14D\uC2A4\uD2B8\uB85C \uC800\uC7A5\uB41C \uD14C\uC774\uBE14 \uB370\
  \uC774\uD130\uB97C \uC704\uD55C \uACF5\uD1B5 \uD615\uC2DD\uC778 CSV \uD30C\uC77C\
  \uC5D0\uC11C \uB370\uC774\uD130\uB97C \uC77D\uACE0 \uC4F0\uB294 \uC791\uC5C5\uC744\
  \ \uD3EC\uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB2E4\
  \uC591\uD55C \uC560\uD50C\uB9AC\uCF00\uC774\uC158, \uB370\uC774\uD130\uBCA0\uC774\
  \uC2A4 \uAC04 \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uAD50\uD658\uD558\uAC70\uB098\
  \ \uB370\uC774\uD130 \uCC98\uB9AC \uBC0F \uBD84\uC11D \uC791\uC5C5\uC744 \uC6A9\uC774\
  \uD558\uAC8C \uD558\uAE30 \uC704\uD574 CSV\u2026"
lastmod: 2024-02-19 22:05:14.116629
model: gpt-4-0125-preview
summary: "CSV(\uCF64\uB9C8\uB85C \uAD6C\uBD84\uB41C \uAC12) \uC791\uC5C5\uC740 \uC77C\
  \uBC18 \uD14D\uC2A4\uD2B8\uB85C \uC800\uC7A5\uB41C \uD14C\uC774\uBE14 \uB370\uC774\
  \uD130\uB97C \uC704\uD55C \uACF5\uD1B5 \uD615\uC2DD\uC778 CSV \uD30C\uC77C\uC5D0\
  \uC11C \uB370\uC774\uD130\uB97C \uC77D\uACE0 \uC4F0\uB294 \uC791\uC5C5\uC744 \uD3EC\
  \uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB2E4\uC591\
  \uD55C \uC560\uD50C\uB9AC\uCF00\uC774\uC158, \uB370\uC774\uD130\uBCA0\uC774\uC2A4\
  \ \uAC04 \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uAD50\uD658\uD558\uAC70\uB098 \uB370\
  \uC774\uD130 \uCC98\uB9AC \uBC0F \uBD84\uC11D \uC791\uC5C5\uC744 \uC6A9\uC774\uD558\
  \uAC8C \uD558\uAE30 \uC704\uD574 CSV\u2026"
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV(콤마로 구분된 값) 작업은 일반 텍스트로 저장된 테이블 데이터를 위한 공통 형식인 CSV 파일에서 데이터를 읽고 쓰는 작업을 포함합니다. 프로그래머들은 다양한 애플리케이션, 데이터베이스 간 데이터를 쉽게 교환하거나 데이터 처리 및 분석 작업을 용이하게 하기 위해 CSV 파일을 조작합니다.

## 방법:

정적으로 타입이 지정된 프로그래밍 언어인 코틀린은 JVM에서 실행되지만, CSV 파일을 처리하기 위한 내장 라이브러리를 포함하고 있지 않습니다. 하지만, Java의 `BufferedReader`와 `FileWriter` 클래스를 기본 작업에 사용하거나, `kotlinx.serialization` 및 `opencsv`와 같은 인기 있는 제3자 라이브러리를 이용하여 보다 고급 기능을 활용할 수 있습니다.

### BufferedReader를 사용하여 CSV 파일 읽기:

```kotlin
import java.io.BufferedReader
import java.io.FileReader

fun main() {
    val path = "data.csv"
    val br = BufferedReader(FileReader(path))
    br.useLines { lines ->
        lines.forEach { line ->
            val cols = line.split(',')
            println(cols)
        }
    }
}
```

_샘플 출력:_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### FileWriter를 사용하여 CSV 파일에 쓰기:

```kotlin
import java.io.FileWriter

fun main() {
    val data = listOf(
        listOf("Name", "Age", "City"),
        listOf("John Doe", "30", "New York"),
        listOf("Jane Smith", "25", "London")
    )

    FileWriter("output.csv").use { writer ->
        data.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

이렇게 하면 제공된 데이터로 `output.csv`를 생성하거나 덮어쓰게 됩니다.

### kotlinx.serialization을 사용한 CSV 시리얼라이제이션:

먼저, `build.gradle.kts`에 의존성을 추가하세요:

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_참고: 올바른 버전과 저장소 설정을 확인하세요._

그 다음, 데이터 클래스를 정의하고 시리얼라이제이션을 위해 `Csv` 형식을 사용하세요:

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Person(val name: String, val age: Int, val city: String)

fun main() {
    val csvFormat = Csv { delimiter = ',' }
    val data = listOf(
        Person("John Doe", 30, "New York"),
        Person("Jane Smith", 25, "London")
    )

    val csvData = csvFormat.encodeToString(data)
    println(csvData)
}
```

_샘플 출력:_

```
John Doe,30,New York
Jane Smith,25,London
```

### 고급 작업을 위한 OpenCSV 사용:

프로젝트의 의존성에 OpenCSV를 추가하세요:

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

OpenCSV를 사용하여 읽기 및 쓰기:

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // CSV 읽기
    CSVReader(FileReader("data.csv")).use { csvReader ->
        val entries = csvReader.readAll()
        entries.forEach { println(it.toList()) }
    }

    // CSV 쓰기
    CSVWriter(FileWriter("output.csv")).use { csvWriter ->
        val entries = listOf(
            arrayOf("Name", "Age", "City"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "London")
        )
        csvWriter.writeAll(entries)
    }
}
```

이 코드 조각들은 코틀린이 CSV 파일 작업을 위해 제공하는 유연성을 보여 주며, 프로젝트의 요구에 가장 잘 맞는 방법을 선택할 수 있게 합니다.
