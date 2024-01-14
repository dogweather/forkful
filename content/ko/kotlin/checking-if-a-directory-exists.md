---
title:                "Kotlin: 디렉터리의 존재 여부 확인하기"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜

파일 또는 디렉토리의 존재 여부를 확인하는 것은 간단한 작업처럼 보일 수 있지만, 실제로는 매우 중요한 기능입니다. 프로그래밍에서는 이러한 존재 여부를 확인할 필요가 있을 때마다 수많은 오류를 방지할 수 있습니다.

# 사용 방법

```Kotlin
val directory = File("/Users/sarah/Documents")
if (directory.exists()) {
    println("디렉토리가 존재합니다.")
} else {
    println("디렉토리가 존재하지 않습니다.")
}
```

위의 코드는 디렉토리가 존재할 경우 "디렉토리가 존재합니다."라는 메시지를 출력하고, 존재하지 않을 경우 "디렉토리가 존재하지 않습니다."라는 메시지를 출력하는 간단한 예시입니다.

또 다른 방법으로는 `File` 클래스의 `exists()` 메서드를 사용하는 것입니다. 이 메서드는 디렉토리가 존재할 경우 `true`를, 존재하지 않을 경우 `false`를 반환합니다.

```Kotlin
val directory = File("/Users/sarah/Pictures")
if (directory.exists()) {
    println("디렉토리가 존재합니다.")
} else {
    println("디렉토리가 존재하지 않습니다.")
}
```

위의 코드는 디렉토리가 존재할 경우 "디렉토리가 존재합니다."라는 메시지를 출력하고, 존재하지 않을 경우 "디렉토리가 존재하지 않습니다."라는 메시지를 출력합니다.

# 깊게 들어가기

`exists()` 메서드를 사용할 때 주의해야 할 점은 해당 디렉토리가 실제로 파일 시스템에서 존재하는지 여부를 확인하는 것입니다. 존재한다고 판단되더라도, 실제로 파일 시스템에는 존재하지 않을 수도 있습니다. 그 이유는 파일 시스템에서 파일이나 디렉토리를 삭제했거나, 다른 프로세스에 의해 이동되거나 이름이 변경되기 때문입니다.

때문에 파일 또는 디렉토리가 존재하는지 확인할 때는 `exists()` 메서드만 사용하는 것이 아니라 추가적인 검사 작업을 수행하는 것이 좋습니다. 예를 들어, 특정 디렉토리 내에 존재하는 파일 목록을 조회하거나, 파일이나 디렉토리를 직접 열어서 존재하는지 확인하는 등의 방법을 사용할 수 있습니다.

# See Also

- [Kotlin Docs: File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/) 
- [Kotlin Docs: File Existence](https://kotlinlang.org/docs/jvm-file-handling.html#file-existence) 
- [GeeksforGeeks: Check if a Directory exists in Kotlin](https://www.geeksforgeeks.org/check-if-a-directory-exists-in-kotlin/)