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

# 왜
폴더가 존재하는지 확인하는 것이 중요한 이유는 프로그램이 동적으로 파일을 조작하고 생성해야 할 때, 사용자에게 오류를 피해주기 위해 필수적이기 때문입니다.

# 방법
```Kotlin
val directory = File("path/to/directory")
if (directory.exists()) {
    println("Directory exists!")
} else {
    println("Directory does not exist.")
}
```

위의 코드는 경로를 지정하여 파일 객체를 생성하고, `exists()` 메소드를 사용하여 폴더가 존재하는지를 확인합니다. 만약 폴더가 존재한다면 "Directory exists!"를 출력하고, 존재하지 않는다면 "Directory does not exist."를 출력합니다.

# 더 들어가기
폴더가 존재하는지 확인하기 위해서는 파일 객체를 생성한 후 `exists()` 메소드를 사용하는 것 외에도 다른 방법들을 검토할 수 있습니다. 예를 들어, `java.nio.file` 패키지에서 제공하는 `Files.exists()` 메소드를 사용할 수도 있습니다. 또는 `directory.isDirectory()`를 사용하여 해당 폴더가 디렉토리인지를 확인할 수도 있습니다.

# 더 알아보기
- [Kotlin - Checking if File or Directory Exists](https://javarevisited.blogspot.com/2019/01/kotlin-check-file-directory-exists.html)
- [How to Check If a Directory Exists in Kotlin](https://attacomsian.com/blog/check-if-directory-exists-kotlin)
- [Kotlin - java.io.File.exists() instead of java.nio.Files.exists() vs kotlin.io.File.exists()](https://stackoverflow.com/questions/54800696/kotlin-java-io-file-exists-instead-of-java-nio-files-exists-vs-kotlin-io-file)
- [Kotlin - java.io.Directory](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#isDirectory--)