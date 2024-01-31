---
title:                "텍스트 파일 읽기"
date:                  2024-01-20T17:55:06.515102-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 파일 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일을 읽는 것은 파일에서 문자 데이터를 가져오는 프로세스입니다. 프로그래머들은 설정, 데이터 분석, 소프트웨어 자동화 등을 위해 이 기능을 사용합니다.

## How to: (어떻게 하나요?)
```Kotlin
// `readText()` 사용하기: 전체 파일 내용을 String으로 읽음
val textFromFile = File("example.txt").readText()
println(textFromFile)

// `forEachLine` 사용하기: 파일의 각 줄을 순회하며 처리
File("example.txt").forEachLine { line ->
    println(line)
}

// `readLines()` 사용하기: 파일의 각 줄을 리스트로 읽음
val lines = File("example.txt").readLines()
lines.forEach { line ->
    println(line)
}
```
**출력 예시**
```
Hello, Kotlin readers!
Learn to read files with ease.
```

## Deep Dive (심층 분석)
텍스트 파일을 읽는 것은 컴퓨터 프로그래밍 초기부터 있어온 기능입니다. 이전에는 낮은 수준의 파일 I/O API로 직접 구현했지만, Kotlin과 같은 현대 언어는 이를 훨씬 쉽게 만듭니다.

`readText()` 같은 고수준 함수를 사용하면 한 줄의 코드로 파일을 읽을 수 있지만, 파일 크기가 클 경우 메모리 문제가 발생할 수 있습니다. 이럴 때 `forEachLine()`이나 `readLines()`를 사용하면 더 효율적으로 대용량 파일을 처리할 수 있습니다.

자바의 `BufferedReader` 같은 클래스를 직접 사용하는 경우도 있지만, Kotlin은 이를 더 단순화한 API를 제공합니다.

## See Also (더 보기)
- [Kotlin Official Documentation on Reading Files](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [BufferedReader in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/)
- [Kotlin File Write](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/write-text.html)
