---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:57:39.731064-07:00
simple_title:         "디렉토리 존재 여부 확인하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

디렉토리의 존재 여부를 확인하는 것은 파일 시스템에서 특정 경로에 폴더가 존재하는지 알아보는 과정입니다. 이 작업은 애플리케이션에서 파일을 저장하거나 읽기 전에 오류를 방지하여 안정성을 높이기 위해 수행됩니다.

## How to (방법):

```kotlin
import java.io.File

fun main() {
    val directoryPath = "path/to/your/directory"
    
    val directory = File(directoryPath)

    if (directory.exists() && directory.isDirectory) {
        println("Directory exists: $directoryPath")
    } else {
        println("Directory does not exist: $directoryPath")
    }
}
```

Sample Output:
```
Directory exists: path/to/your/directory
```
OR
```
Directory does not exist: path/to/your/directory
```

## Deep Dive (심층 분석):

역사적으로 자바의 `File` 클래스는 디렉토리와 파일 작업의 핵심이었습니다. Kotlin에서도 `java.io.File`을 가져와 사용합니다. Kotlin은 Java 라이브러리와 상호 운용할 수 있기 때문에 Java의 기능을 그대로 활용할 수 있습니다. 대안으로, Kotlin의 멀티플랫폼 프로젝트에서는 Kotlin/Native의 파일 시스템 기능을 사용할 수 있습니다. 

`File.exists()` 메서드는 파일 또는 디렉토리가 실제로 존재하는지 확인합니다. `isDirectory` 속성은 해당 경로가 파일이 아닌 디렉토리인지를 추가로 확인합니다. `isFile` 속성을 사용해 경로가 파일인 경우를 확인할 수도 있습니다. 

`File`을 사용해 디렉토리 검사를 할 때 주의할 점은 심볼릭 링크를 처리하는 방식입니다. 심볼릭 링크가 가리키는 대상이 존재하지 않으면 `exists()`는 `false`를 반환합니다.

## See Also (더 보기):

- Kotlin documentation on File operations: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- Java File Class documentation: [https://docs.oracle.com/javase/7/docs/api/java/io/File.html](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- Kotlin/Native documentation: [https://kotlinlang.org/docs/native-overview.html](https://kotlinlang.org/docs/native-overview.html)
