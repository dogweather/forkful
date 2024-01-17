---
title:                "텍스트 파일 읽기"
html_title:           "Kotlin: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
텍스트 파일 읽기란 무엇일까요? 텍스트 파일 읽기는 파일의 내용을 읽어오는 것을 말합니다. 프로그래머들은 텍스트 파일을 읽는 이유는 다양합니다. 예를 들어, 사용자의 입력을 받기 위해 설정 파일을 읽는 경우나, 데이터베이스의 쿼리 결과를 파일로 저장하기 위해 사용하는 경우 등이 있습니다.

## 방법:
```kotlin
fun main() {
    val file = File("data.txt") // 파일 경로 설정
    val lines = file.readLines() // 파일의 각 줄을 리스트로 읽어옴
    lines.forEach { line ->
        println(line) // 각 줄을 출력
    }
}
```

**결과:**
```
첫 번째 줄
두 번째 줄
세 번째 줄
네 번째 줄
```

## 딥 다이브:
텍스트 파일 읽기는 오랜 역사를 가지고 있습니다. 초기 컴퓨터들은 데이터를 저장하기 위해 테이프를 사용했는데, 이 테이프들은 텍스트 파일을 읽고 쓸 수 있었습니다. 하지만 현재는 텍스트 파일 뿐만 아니라 다양한 형식의 파일도 읽고 쓸 수 있습니다. 또 텍스트 파일을 읽는 대안으로는 바이너리 파일 읽기가 있습니다. 텍스트 파일은 사람이 읽고 쓰기 쉽지만 바이너리 파일은 기계가 빠르게 처리할 수 있습니다. 텍스트 파일을 읽는 방법은 운영체제마다 다를 수 있지만, 코틀린에서는 `File` 클래스를 사용하면 쉽게 파일을 읽고 쓸 수 있습니다.

## 참고하기:
- [코틀린 공식 문서 - 파일 읽고 쓰기](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/index.html)
- [텍스트 파일 vs 바이너리 파일](https://ko.wikipedia.org/wiki/%ED%85%8D%EC%8A%A4%ED%8A%B8_%ED%8C%8C%EC%9D%BC)
- [오래된 컴퓨터들의 데이터 저장 방식](https://ko.wikipedia.org/wiki/%EC%9A%B0%EB%A6%AC%EC%98%A8_%EC%BB%B4%ED%93%A8%ED%84%B0_%EC%8A%A4%ED%86%A0%EB%A6%AC%EC%A0%84_%EC%A0%84%EC%9B%90)