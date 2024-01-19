---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디렉토리가 존재하는지 확인하는 것은 파일 경로가 실제로 존재하고 액세스 가능한지 체크하는 프로그래밍 방법입니다. 이것이 필요한 이유는 무효한 경로에 접근하면서 오류를 피하기 위함입니다.

## 어떻게:
```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun isDirectoryExists(path: String): Boolean {
    return Files.exists(Paths.get(path))
}

fun main() {
    println(isDirectoryExists("/path/to/directory"))  // Output: true 혹은 false
}
```
위 코드는 주어진 경로가 존재하는지를 판별하고, 그 결과를 출력합니다. 경로가 존재하면 `true`, 그렇지 않으면 `false`를 출력합니다.

## 깊은 탐색
디렉토리 확인은 올드 스쿨한 프로그래밍 기법 중 하나로 파일 시스템에 직접적인 영향을 미치는 작업입니다. 

대안으로, `java.io.File` 클래스의 `exists()` 메소드를 사용할 수 있습니다. 하지만 이 방법은 오래됐으며, 자바 7부터는 `java.nio.file` 패키지를 이용하는 것이 권장됩니다. 

`Files.exists(Paths.get(path))` 코드 실행 시, 실제로는 파일 시스템의 특정 경로로 접근하여 그 경로가 존재하는지를 확인하게 됩니다. 디렉토리 존재 확인은 단순히 체크만 수행하는 것이 아니라, 나중에 해당 경로를 사용한 작업이 안전하게 수행될 수 있도록 합니다.

## 참고 자료
1. [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/)
2. [Oracle Java NIO 문서](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html#exists-java.nio.file.Path-java.nio.file.LinkOption...-)
3. [Oracle Java IO 문서](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)
4. [Kotlin과 자바의 파일 핸들링 비교](http://www.differencebetween.net/technology/difference-between-kotlin-and-java/#:~:text=Difference%20Between%20Kotlin%20and%20Java)
5. [StackOverflow: 디렉터리 존재 확인 방법 비교](https://stackoverflow.com/questions/3775694/checking-if-a-folder-exists-in-java)