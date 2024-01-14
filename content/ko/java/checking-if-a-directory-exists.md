---
title:                "Java: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

일반적으로 우리는 개발 프로젝트에서 디렉토리를 생성하고 사용합니다. 그리고 때로는 특정 디렉토리가 존재하는지 여부를 확인해야 할 때가 있습니다. 이 글에서는 Java에서 디렉토리가 존재하는지 확인하는 방법을 알려드리겠습니다.

## 방법

디렉토리가 존재하는지 확인하는 가장 간단한 방법은 `File` 클래스의 `exists()` 메소드를 사용하는 것입니다. 이 메소드는 boolean 형태로 디렉토리가 존재하는지 여부를 확인할 수 있습니다. 아래는 예시 코드와 출력 결과입니다.

```Java
import java.io.File;

public class CheckDirectory {
    public static void main(String[] args) {
        File directory = new File("mydirectory");
        if (directory.exists()) {
            System.out.println("mydirectory가 존재합니다.");
        } else {
            System.out.println("mydirectory가 존재하지 않습니다.");
        }
    }
}
```

출력 결과:

```
mydirectory가 존재하지 않습니다.
```

위 예시에서 `mydirectory`는 존재하지 않는 디렉토리이기 때문에 존재하지 않는다는 결과가 나타납니다. 만약 존재하는 디렉토리인 경우 `mydirectory가 존재합니다.`라는 결과가 나타날 것입니다.

## 깊게 파보기

Java 7부터는 `Files` 클래스를 이용하여 디렉토리를 확인할 수 있습니다. `Files.exist(Path path)`를 사용하여 boolean 형태로 디렉토리가 존재하는지 여부를 확인할 수 있습니다. 아래는 예시 코드와 출력 결과입니다.

```Java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class CheckDirectory {
    public static void main(String[] args) {
        Path directory = Paths.get("mydirectory");
        if (Files.exists(directory)) {
            System.out.println("mydirectory가 존재합니다.");
        } else {
            System.out.println("mydirectory가 존재하지 않습니다.");
        }
    }
}
```

출력 결과:

```
mydirectory가 존재하지 않습니다.
```

위 예시에서도 마찬가지로 `mydirectory`가 존재하지 않기 때문에 존재하지 않는다는 결과가 나타납니다.

## 더 알아보기

위에서는 디렉토리가 존재하는지 여부를 확인하는 방법을 알아보았는데, 만약 디렉토리가 존재하지 않는 경우에는 디렉토리를 생성하는 방법도 알아볼 수 있습니다. 또한 디렉토리의 경로를 확인하여 현재 위치하는 디렉토리를 알아보는 방법도 있습니다.

## 더 알아보기

- [Java File class documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java Files class documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html)
- [How to create a directory in Java](https://www.baeldung.com/java-create-directory)
- [Getting current directory in Java](https://www.baeldung.com/java-current-directory)