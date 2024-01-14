---
title:    "Java: 디렉토리가 존재하는지 확인하는 방법"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것이 왜 중요한지 궁금하셨을까요? 그 이유는 미리 디렉토리 여부를 파악하면 예기치 않은 에러를 방지할 수 있기 때문입니다. 예를 들어, 프로그램에서 파일을 읽거나 쓰기 전에 디렉토리가 있는지 확인한다면 파일이 존재하지 않을 때 생기는 오류를 방지할 수 있습니다. 

## 어떻게

```Java
import java.io.File;

public class DirectoryCheck{
    public static void main(String[] args) {
        // 존재하는 디렉토리의 경로를 지정합니다.
        File directory = new File("/Users/username/Documents");

        // 디렉토리가 존재하는지 확인합니다.
        if(directory.exists()){
            System.out.println("지정한 디렉토리는 존재합니다.");
        } else {
            System.out.println("지정한 디렉토리는 존재하지 않습니다.");
        }
    }
}
```

위의 예제 코드에서는 `java.io.File` 클래스의 `exists()` 메소드를 사용하여 디렉토리가 존재하는지 확인합니다. `exists()` 메소드는 디렉토리가 존재할 경우 `true`를, 존재하지 않을 경우 `false`를 반환합니다. 이를 통해 `if-else` 문을 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다. 

```Java
// 존재하지 않는 디렉토리의 경로를 지정합니다.
File directory = new File("/Users/username/Downloads");

// 디렉토리가 존재하지 않을 경우 예외 메세지를 출력합니다.
if(!directory.exists()){
    System.out.println("지정한 디렉토리는 존재하지 않습니다.");
}
```

위의 코드에서는 `!` 연산자를 사용하여 `if` 문을 사용하지 않고도 디렉토리가 존재하지 않을 경우를 처리할 수 있습니다. 존재하지 않는 디렉토리를 지정한 경우에는 예외 메세지를 출력하므로 프로그램이 오류 없이 정상적으로 작동할 수 있습니다.

## 깊게 파고들기

디렉토리가 존재하는지 확인하는 방법은 간단하지만, 실제로는 디렉토리 경로를 확인하는 과정에서 발생하는 일련의 메소드 호출과 파일 시스템의 접근을 거치게 됩니다. 이를 통해 운영 체제와 상호작용하여 디렉토리가 존재하는지 여부를 확인합니다. 따라서 디렉토리가 존재하는지 확인하지 않는다면 프로그램이 오류 없이 작동하기 어려울 수 있습니다.

## 참고 자료

- [Java File Class - Oracle Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java File I/O (입출력) - W3Schools](https://www.w3schools.com/java/java_files.asp)
- [File System - Tutorials Point](https://www.tutorialspoint.com/java/java_file_system.htm)