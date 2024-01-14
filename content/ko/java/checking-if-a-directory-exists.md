---
title:                "Java: 디렉토리가 존재하는지 확인하기"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

특정 디렉토리가 존재하는지 확인하는 것의 이유는, 파일을 검색하거나 처리하기 전에 해당 디렉토리가 존재하는지를 미리 확인하여 프로그램이 예외를 발생시키지 않도록 하기 위함입니다.

## 어떻게

자바에서 디렉토리가 존재하는지 확인하는 방법은 간단합니다. 먼저, `java.io.File` 클래스의 `exists()` 메소드를 사용하면 됩니다. 이 메소드는 해당 경로가 존재할 경우 `true`를, 존재하지 않을 경우 `false`를 반환합니다. 아래는 이를 사용한 예제 코드입니다.

```Java
import java.io.File;

public class DirectoryCheckExample {

    public static void main(String[] args) {

        File directory = new File("C:\\Users\\User\\Documents");
        if(directory.exists()) {
            System.out.println("해당 디렉토리가 존재합니다.");
        } else {
            System.out.println("해당 디렉토리가 존재하지 않습니다.");
        }

    }

}
```

위 코드를 실행하면, "해당 디렉토리가 존재합니다."라는 메시지가 출력됩니다. 또한, 존재하지 않는 디렉토리 경로를 입력할 경우 "해당 디렉토리가 존재하지 않습니다."라는 메시지가 출력됩니다.

## 깊게 들어가보기

`java.io.File` 클래스는 파일 뿐만 아니라 디렉토리의 존재도 확인할 수 있기 때문에 `exists()` 메소드는 매우 유용합니다. 이 메소드를 사용하면 디렉토리를 검색하거나 파일을 다룰 때 예외를 처리하는 등의 유용한 기능을 구현할 수 있습니다.

## 참고 자료

- [The `File` Class in Java](https://www.baeldung.com/java-file)
- [Checking if a Directory Exists in Java](https://www.javadevjournal.com/java/checking-if-a-directory-exists-in-java/)