---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:28.081357-07:00
description: "\uCF54\uD2C0\uB9B0\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744\
  \ \uC791\uC131\uD558\uB294 \uAC83\uC740 \uD30C\uC77C\uC744 \uC0DD\uC131\uD558\uACE0\
  \ \uADF8 \uC548\uC5D0 \uD14D\uC2A4\uD2B8 \uB0B4\uC6A9\uC744 \uC785\uB825\uD558\uB294\
  \ \uAC83\uC744 \uB9D0\uD558\uBA70, \uB370\uC774\uD130 \uC800\uC7A5, \uB85C\uAE45\
  \ \uB610\uB294 \uAD6C\uC131 \uC124\uC815\uC744 \uC704\uD55C \uC77C\uBC18\uC801\uC778\
  \ \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\
  \uB97C \uD1B5\uD574 \uD718\uBC1C\uC131 \uBA54\uBAA8\uB9AC \uACF5\uAC04 \uC678\uBD80\
  \uC5D0\uC11C \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD558\uACE0 \uC870\uC791\uD558\
  \uC5EC \uC138\uC158 \uAC04\uC5D0 \uB370\uC774\uD130\uAC00 \uC9C0\uC18D\uB418\uB3C4\
  \uB85D\u2026"
lastmod: '2024-03-13T22:44:55.198540-06:00'
model: gpt-4-0125-preview
summary: "\uCF54\uD2C0\uB9B0\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\
  \uC131\uD558\uB294 \uAC83\uC740 \uD30C\uC77C\uC744 \uC0DD\uC131\uD558\uACE0 \uADF8\
  \ \uC548\uC5D0 \uD14D\uC2A4\uD2B8 \uB0B4\uC6A9\uC744 \uC785\uB825\uD558\uB294 \uAC83\
  \uC744 \uB9D0\uD558\uBA70, \uB370\uC774\uD130 \uC800\uC7A5, \uB85C\uAE45 \uB610\uB294\
  \ \uAD6C\uC131 \uC124\uC815\uC744 \uC704\uD55C \uC77C\uBC18\uC801\uC778 \uC791\uC5C5\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\
  \uD574 \uD718\uBC1C\uC131 \uBA54\uBAA8\uB9AC \uACF5\uAC04 \uC678\uBD80\uC5D0\uC11C\
  \ \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD558\uACE0 \uC870\uC791\uD558\uC5EC \uC138\
  \uC158 \uAC04\uC5D0 \uB370\uC774\uD130\uAC00 \uC9C0\uC18D\uB418\uB3C4\uB85D\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 무엇 & 왜?
코틀린에서 텍스트 파일을 작성하는 것은 파일을 생성하고 그 안에 텍스트 내용을 입력하는 것을 말하며, 데이터 저장, 로깅 또는 구성 설정을 위한 일반적인 작업입니다. 프로그래머들은 이를 통해 휘발성 메모리 공간 외부에서 데이터를 저장하고 조작하여 세션 간에 데이터가 지속되도록 합니다.

## 어떻게:
코틀린은 추가적인 제3자 라이브러리가 필요 없이 표준 라이브러리를 활용하여 파일에 쓰기 위한 간단한 접근 방법을 제공합니다. 다음은 간단한 예제입니다:

```kotlin
import java.io.File

fun main() {
    val textToWrite = "안녕하세요, 코틀린 파일 쓰기!"
    File("example.txt").writeText(textToWrite)
}
```
이 코드 조각은 프로젝트의 루트 디렉터리에 "example.txt"라는 이름의 파일을 생성하고 그 안에 `안녕하세요, 코틀린 파일 쓰기!` 라는 문자열을 작성합니다. 파일이 이미 존재하는 경우, 덮어쓰기됩니다.

파일에 추가하거나 더 많은 양의 데이터를 쓰려면 `appendText` 또는 `bufferedWriter()`를 사용할 수 있습니다:

```kotlin
import java.io.File

fun appendToFile() {
    val moreText = "더 많은 텍스트 추가."
    File("example.txt").appendText(moreText)
}

fun writeWithBufferedWriter() {
    val largeText = "대량의 텍스트...\n여러 줄에 걸쳐."
    File("output.txt").bufferedWriter().use { out ->
        out.write(largeText)
    }
}

fun main() {
    appendToFile() // 기존 파일에 텍스트 추가
    writeWithBufferedWriter() // 대량의 텍스트 데이터를 효율적으로 작성
}
```

`appendToFile` 함수에서는 "example.txt"에 현재 내용을 덮어쓰지 않고 더 많은 텍스트를 추가합니다. `writeWithBufferedWriter` 함수는 대량의 텍스트나 데이터를 쓰는 효율적인 방법을 보여주며, 특히 여러 줄 또는 대형 파일을 다룰 때 I/O 작업을 최소화하기 위해 유용합니다.

이 예제들은 코틀린에서 텍스트 파일을 작성하기 위한 기본 작업을 다루며, 파일 I/O 작업을 위한 코틀린의 표준 라이브러리의 간단함과 강력함을 보여줍니다.
