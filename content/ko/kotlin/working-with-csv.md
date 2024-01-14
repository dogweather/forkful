---
title:                "Kotlin: csv 파일로 작업하는 법"
simple_title:         "csv 파일로 작업하는 법"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

Kotlin에서 CSV 작업을 수행하는 것에 대해

## 왜?
CSV(Comma Separated Values)는 데이터 저장에 매우 유용한 형식입니다. 따라서 Kotlin 프로그래밍 언어를 사용하여 CSV 파일을 읽고 쓰는 것은 매우 유용합니다.

## 방법
```kotlin
// CSV 파일 읽기
val file = File("data.csv")
val csvReader = file.bufferedReader()
// 데이터를 저장할 리스트 생성
val data = mutableListOf<List<String>>()
// 각 줄을 읽고 쉼표로 나눈 다음 리스트에 추가
csvReader.forEachLine {
   data.add(it.split(","))
}
// 첫번째 행은 열 이름이 될 것이므로 제거
val headers = data.removeAt(0)
// 데이터를 읽은 결과 출력
println(headers)
println(data)

// CSV 파일 쓰기
val fileWriter = File("output.csv").bufferedWriter()
// 첫번째 행에 열 이름 쓰기
fileWriter.write(headers.joinToString(","))
// 각 행마다 데이터 쓰기
for(row in data) {
   fileWriter.newLine()
   fileWriter.write(row.joinToString(","))
}
fileWriter.close()

// 출력 결과
[Name, Age, Gender]
[[John, 28, Male], [Jane, 23, Female], [Alex, 32, Male]]
```

## 더 깊게 들어가기
CSV 파일을 읽고 쓰는 것 이외에도 Kotlin에서는 다양한 추가 기능을 제공합니다. 예를 들어, `BufferedReader` 클래스를 사용하여 파일의 첫번째 줄을 건너뛰고 데이터를 읽는 것이 가능합니다. 또한 `split()` 함수를 사용하여 특정 문자로 문자열을 분리하는 것도 가능합니다.

## 자세한 내용은 다음을 참조하세요
- [Kotlin CSV 라이브러리 확인하기](https://github.com/doyeongkim/KotlinCSV)
- [Kotlin의 FileReader와 BufferedReader 사용하기](https://www.javatpoint.com/kotlin-read-file)
- [Kotlin 문자열 다루기](https://kotlinlang.org/docs/basic-types.html#string-literals)