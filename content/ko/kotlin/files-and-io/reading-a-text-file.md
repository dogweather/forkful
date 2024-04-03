---
date: 2024-01-20 17:55:06.515102-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.196999-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

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
