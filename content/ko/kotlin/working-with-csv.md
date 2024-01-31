---
title:                "CSV 파일 다루기"
date:                  2024-01-19
simple_title:         "CSV 파일 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV(Comma-Separated Values)는 값들이 쉼표로 구분된 텍스트 파일이다. 프로그래머는 대량의 데이터를 손쉽게 읽고 쓰기 위해 CSV를 사용한다.

## How to:
데이터를 읽고 쓰는 법.

```Kotlin
import java.io.File

fun main() {
    // CSV 파일 쓰기
    val dataToWrite = listOf(
        listOf("id", "name", "age"),
        listOf("1", "Kim", "25"),
        listOf("2", "Lee", "30")
    )
    
    File("data.csv").printWriter().use { out ->
        dataToWrite.forEach { line ->
            out.println(line.joinToString(","))
        }
    }
    
    // CSV 파일 읽기
    val readData = File("data.csv").readLines().map { it.split(",") }
    readData.forEach { println(it) }
}
```

Sample Output:
```
[id, name, age]
[1, Kim, 25]
[2, Lee, 30]
```

## Deep Dive
CSV 형식은 1970년대부터 사용되고 있다. XML이나 JSON과 같은 다른 데이터 형식과 비교하면, CSV는 구문 분석이 더 간단하고 소량의 메타데이터를 포함한다. 하지만 복잡한 구조를 표현하기에는 제한적이다.

## See Also
- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/)
- [OpenCSV 라이브러리](http://opencsv.sourceforge.net/)
- [RFC 4180, CSV 표준](https://tools.ietf.org/html/rfc4180)
