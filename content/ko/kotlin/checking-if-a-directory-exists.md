---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Kotlin: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디렉토리가 존재하는지 확인하는 것은 파일 시스템에서 매우 일반적인 작업입니다. 프로그래머들은 이를 사용하여 디렉토리가 이미 존재하는지 확인하고, 필요한 경우 새로운 디렉토리를 생성할 수 있습니다.

## 방법:
### 디렉토리 존재 여부 확인:
```Kotlin
val directory = File("/path/to/directory")
if (directory.exists()) {
    println("Directory exists!")
} else {
    println("Directory does not exist.")
}
```

### 새로운 디렉토리 생성:
```Kotlin
val directory = File("/path/to/new/directory")
if (directory.mkdir()) {
    println("Directory created successfully!")
} else {
    println("Failed to create directory.")
}
```

## 깊이 들어가보기:
### 역사적 배경:
디렉토리 존재 여부 확인은 오래된 개념이며, 다양한 운영체제에서 사용되었습니다. 예를 들어, 윈도우 운영체제에서는 `dir` 명령어를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 리눅스 운영체제에서는 `ls` 명령어를 사용합니다.

### 대안:
`File` 클래스의 `exists()` 메서드 외에도, `Files` 클래스의 `exists()` 메서드를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 또한 `listFiles()` 메서드를 사용하여 디렉토리에 포함된 파일 및 디렉토리의 리스트를 가져올 수도 있습니다.

### 구현 세부 사항:
`File` 클래스의 `exists()` 메서드는 해당 파일을 생성하거나 수정할 수 있는 권한이 있는지 검사합니다. 이를 통해 사용자가 여러 가지 작업을 수행할 수 있도록 보호합니다.

## 관련 자료:
- [Java Documentation on File and Directory Operations](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Kotlin，주석](https://kotlinlang.org/spec/documentation.html)