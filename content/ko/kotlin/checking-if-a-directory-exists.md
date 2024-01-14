---
title:                "Kotlin: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
*이 프로그래밍 블로그 게시물은 코틀린을 공부하는 한국 독자들을 위한 것입니다.*

어떤 경우에 우리는 디렉토리가 이미 존재하는지 확인해야 할까요? 파일을 읽거나 저장하려면 존재하는 디렉토리에 액세스할 수 있어야 합니다. 또는 파일을 생성하거나 수정하려면 해당 디렉토리가 존재하는지 확인해야 합니다. 이러한 이유로, 디렉토리의 존재 여부를 확인하는 것은 중요합니다.

## 어떻게

디렉토리가 존재하는지 확인하는 가장 간단한 방법은 Java의 `File` 클래스를 사용하는 것입니다. `File` 클래스는 존재하는 파일이나 디렉토리를 나타내는데 사용됩니다. 코틀린에서 `File` 클래스를 사용하여 디렉토리가 존재하는지 확인하는 예제를 살펴보겠습니다.

```Kotlin
import java.io.File

fun main() {
    val directory = File("파일 경로")
    if (directory.exists()) {
        println("디렉토리가 존재합니다.")
    } else {
        println("디렉토리가 존재하지 않습니다.")
    }
}
```

위의 예제에서는 `exists()` 메소드를 사용하여 디렉토리가 존재하는지 확인합니다. 이 메소드는 `true` 또는 `false` 값을 반환합니다. 만약 디렉토리가 존재하지 않는다면 `else` 블록이 실행되어 "디렉토리가 존재하지 않습니다."라는 메시지를 출력합니다.

## 그 이상으로

`exists()` 메소드를 사용하는 것 이외에도, 코틀린에서는 `Files` 클래스를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. `Files` 클래스는 JDK의 `java.nio.file` 패키지에 속한 메소드를 사용할 수 있도록 해줍니다. 예를 들어 다음과 같이 `Files.exists()` 메소드를 사용할 수 있습니다.

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("파일 경로")
    if (Files.exists(path)) {
        println("디렉토리가 존재합니다.")
    } else {
        println("디렉토리가 존재하지 않습니다.")
    }
}
```

`File` 클래스는 JDK 10부터는 사라졌지만 `Files` 클래스는 JDK 11에서도 계속 사용할 수 있습니다.

## See Also
* [Kotlin File class documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
* [Java Files class documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)