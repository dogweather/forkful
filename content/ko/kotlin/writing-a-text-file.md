---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
텍스트 파일 작성은 데이터를 영구적으로 저장하는 행위입니다. 프로그래머는 로그 기록, 사용자 데이터 저장, 설정 이동 등을 위해 이 기술을 사용합니다.

## How to: (어떻게:)
```Kotlin
import java.io.File

fun main() {
    val data = "안녕하세요!\nKotlin 파일 작성 예제입니다."
    File("example.txt").writeText(data)
    println("파일 작성 완료!")
}
```
출력:
```
파일 작성 완료!
```

## Deep Dive (자세히 알아보기)
Kotlin은 Java의 표준 라이브러리를 사용하여 텍스트 파일을 씁니다. `java.io.File`은 `writeText` 함수를 제공합니다. 큰 파일이나 예외 처리가 필요한 상황에서는 BufferedWriter를 사용할 수도 있습니다. `FileWriter` 또는 `PrintWriter` 같은 Java의 대안적인 방법들도 있습니다. Kotlin이 편의성을 높이기 위해 `File` 클래스를 확장해 `writeText` 함수를 제공함에도, 내부에서 Java의 BufferedWriter를 사용하여 구현됩니다.

## See Also (참고 자료)
- [Kotlin Documentation: Writing files](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/write-text.html)
- Java의 `FileWriter` 클래스: [Java FileWriter Class](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
