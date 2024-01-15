---
title:                "텍스트 파일 작성하기"
html_title:           "Kotlin: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜
* 왜 누군가가 텍스트 파일을 작성하게 되는지 이유를 설명합니다.
* 텍스트 파일은 컴퓨터에서 매우 중요한 역할을 하며, 데이터를 저장하고 공유하기 위해서 빠질 수 없는 도구입니다.

## 방법
텍스트 파일을 작성하는 방법을 간단한 코틀린 예제와 함께 알아보겠습니다. 

```kotlin
fun main() {
    val text = "Hello, World!" // 작성할 텍스트
    val file = File("newfile.txt") // 새로운 파일 생성
    
    file.writeText(text) // 파일에 텍스트 작성
    println("파일 작성이 완료되었습니다.")
    
    val readText = file.readText() // 파일 읽기
    println("파일 내용: $readText")
}
```

**새로운 파일 생성**
```kotlin
val file = File("newfile.txt")
```

**파일에 텍스트 작성**
```kotlin
file.writeText(text)
```

**파일 읽기**
```kotlin
val readText = file.readText()
```

**새로운 텍스트 파일이 생성되고, 파일에 내용이 작성되며, 파일을 읽을 수 있습니다.**

## 깊이 들어가기
텍스트 파일을 작성하는 데에 있어서 더 깊이있게 이해해보겠습니다.

**파일 경로 지정하기**
```kotlin
val file = File("newfile.txt") // 현재 디렉토리에 파일 생성
```
현재 디렉토리가 아닌 다른 경로에 파일을 생성하려면, 경로를 포함한 파일명을 지정해줘야 합니다.
```kotlin
val file = File("C:\\Users\\User1\\Desktop\\newfile.txt")
```

**파일 작성 모드 설정하기**
파일 작성 모드는 기본적으로 덮어쓰기 모드(`WRITE_MODE`)로 설정되어 있습니다. 즉, 파일을 생성하거나 기존 내용을 모두 삭제하고 새로운 내용을 작성합니다. 하지만, 파일을 이어쓰기 모드(`APPEND_MODE`)로 설정할 수도 있습니다.
```kotlin
file.writeText(text, APPEND_MODE) // 파일에 텍스트 추가
```

**파일 삭제하기**
파일 삭제는 `delete()` 함수를 사용하면 됩니다.
```kotlin
file.delete() // 파일 삭제
```

## 또한 보기
* [Kotlin 텍스트 파일 쓰기](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/write-text.html)
* [Kotlin 파일 다루기](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-io.html)

파일을 작성하는 방법에 대해 알아보았습니다. 이제 여러분도 코틀린을 사용해 손쉽게 텍스트 파일을 작성할 수 있습니다!