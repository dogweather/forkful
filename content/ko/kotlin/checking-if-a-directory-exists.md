---
title:    "Kotlin: 디렉토리의 존재 유무 확인하기"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜 필요할까요?

우리가 가끔 디렉토리가 존재하는지를 확인해야 할 때가 있습니다. 이를 확인하는 방법을 알고 있다면, 애플리케이션을 보다 효율적으로 관리할 수 있습니다. 이 글에서는 Kotlin에서 디렉토리가 존재하는지를 확인하는 방법을 배워보겠습니다.

## 방법

먼저 사용할 메소드를 import 합니다. 다음 예시는 특정 디렉토리가 존재하는지를 확인하는 방법입니다.

```Kotlin
import java.io.File

fun main() {
  val directory = File("C:/Users/username/Desktop/myDirectory")
  println(directory.exists())
}
```
위 코드에서 `File` 클래스를 import 하고 `main()` 함수 안에서 특정 디렉토리 경로를 `File` 객체로 만든 후 `exists()` 메소드를 호출하면 해당 디렉토리가 존재하는지를 확인할 수 있습니다. 예시 코드의 결과는 `true`나 `false`로 나타납니다.

## 딥 다이브

`File` 클래스에는 디렉토리 외에도 파일이 존재하는지를 확인하는 `isFile()`과 하위 디렉토리가 있는지를 확인하는 `isDirectory()` 메소드가 있습니다. 또한, `exists()` 메소드를 이용하여 다른 경로에도 사용할 수 있습니다. 예를 들어, 다음과 같이 `directory.exists()` 대신 `File("C:/Users/username/Desktop/myDirectory/myFile.txt").exists()`를 사용하여 파일이 존재하는지를 확인할 수 있습니다.

## 더 알아보기

- [Kotlin API 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Kotlin - 디렉토리와 파일 다루기](https://codechacha.com/ko/kotlin-manage-directory-file/)
- [Kotlin에서 파일 입출력하기](https://www.baeldung.com/kotlin-file-io)