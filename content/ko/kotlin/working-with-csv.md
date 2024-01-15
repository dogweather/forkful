---
title:                "컴퓨터 프로그래밍에 대한 기사 제목: csv와 함께 작업하기"
html_title:           "Kotlin: 컴퓨터 프로그래밍에 대한 기사 제목: csv와 함께 작업하기"
simple_title:         "컴퓨터 프로그래밍에 대한 기사 제목: csv와 함께 작업하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

Why: CSV(Comma Separated Values)는 데이터를 저장하고 교환하기 위해 널리 사용되는 파일 형식입니다. CSV를 사용하여 데이터를 읽고 쓸 수 있으면 데이터 처리 작업이 더 쉬워집니다.

## Why
데이터 처리를 위해 CSV를 사용하는 것은 매우 일반적입니다. 나이, 성별, 위치와 같은 간단한 정보를 포함하는 데이터를 처리해야 하는 상황이 발생할 수 있으며, 이러한 경우 CSV 파일 형식이 매우 유용합니다. 또한 CSV는 표 형식으로 데이터를 저장하므로 데이터베이스에 데이터를 저장하고 검색하는 것보다 간단하고 직관적입니다.

## How To
```Kotlin
// CSV 파일을 읽는 예제 코드
val file = File("sample.csv")

file.bufferedReader().forEachLine {
    val data = it.split(",") // 쉼표로 분리된 데이터를 읽음
    println(data[0]) // 첫 번째 열 데이터 출력
    println(data[1]) // 두 번째 열 데이터 출력
}

/*
콘솔에 출력 결과:
John
26
Sarah
32
*/

// CSV 파일을 쓰는 예제 코드
val file = File("new_sample.csv")

file.printWriter().use { out ->
    out.println("Name,Age")
    out.println("Mike,40")
    out.println("Emily,28")
}

/*
new_sample.csv 파일의 내용:
Name,Age
Mike,40
Emily,28
*/
```

## Deep Dive
CSV 파일은 각 열마다 쉼표로 구분된 데이터를 포함합니다. 때문에 데이터에 쉼표가 포함되어 있으면 문제가 발생할 수 있습니다. 이러한 경우에는 쉼표 대신 다른 구분자를 사용하거나, 해당 데이터를 따옴표로 감싸주는 등의 방법을 사용하여 문제를 해결할 수 있습니다. 또한 CSV 파일을 읽거나 쓰기 전에 데이터 타입 변환이 필요한 경우가 있습니다. 이는 Kotlin의 `toInt()`, `toFloat()` 등의 함수를 사용하여 간단하게 해결할 수 있습니다.

## See Also
- [Kotlin CSV Parser](https://github.com/doyaaaaaken/kotlin-csv)
- [Reading and Writing CSV Files in Kotlin](https://www.baeldung.com/kotlin/csv)
- [CSV File Handling in Kotlin](https://www.geeksforgeeks.org/csv-file-handling-in-kotlin/)