---
title:                "임시 파일 생성하기"
html_title:           "Kotlin: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 뭔가 & 왜?
임시 파일을 만드는 것은, 임시적인 데이터를 저장하고 사용하기 위해 프로그래머들이 하는 작업입니다. 이런 방식으로, 우리는 필요한 데이터를 보관하고 재사용할 수 있게 됩니다.

## 하다 방법:
```Kotlin
// 새로운 임시 파일을 생성합니다.
val tempFile = kotlin.io.createTempFile()

// 파일 이름 매개 변수를 전달하여 특정 파일 이름으로 임시 파일을 생성합니다.
val customTempFile = kotlin.io.createTempFile("custom", ".txt")

// 임시 파일을 생성할 디렉토리를 지정합니다.
val customDirectory = File("myDirectory")
val customTempFile2 = kotlin.io.createTempFile("custom", ".txt", customDirectory)

// 임시 파일에 데이터를 쓰고 읽어올 수 있습니다.
tempFile.writeBytes("Hello World".toByteArray())
val data = customTempFile.readBytes()

// 마지막으로, 사용이 끝나면 임시 파일을 삭제할 수 있습니다.
tempFile.delete()
```

## 깊은 글:
장기적으로 새로운 파일이 필요한 작업에서는, 항상 디스크 상에 파일을 만들어야 합니다. 하지만 임시 파일은 우리가 프로그램을 종료할 때 자동으로 삭제되어 용량을 절약해줍니다. 또한, 이 파일들을 사용하는 모든 프로세스에서는 이 파일들에 쓰고 읽을 수 있습니다.

다른 대안으로는, 우리는 ```Files.createTempFile()``` 메서드를 사용할 수 있습니다. 이 메서드는 자바에서 제공하는 임시 파일 생성 방법입니다. 또한 우리는 파일 생성에 사용된 코드가 무엇을 하는지 이해하기 쉽고 읽기 쉬운 방법으로 작성할 수 있도록, 코틀린 확장 함수를 사용할 수도 있습니다.

마지막으로, 코틀린은 [Java's NIO library](https://www.baeldung.com/java-nio)를 사용하여 파일을 생성하기 때문에, 다양한 언어에서도 임시 파일을 생성하는 방법을 배울 수 있습니다.

## 관련 자료:
- [Kotlin의 임시 파일 생성](https://www.programiz.com/kotlin-programming/examples/create-temp-file)
- [Java에서의 임시 파일 생성](https://www.baeldung.com/java-temporary-file)