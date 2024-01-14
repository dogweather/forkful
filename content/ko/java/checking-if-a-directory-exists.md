---
title:    "Java: 디렉토리가 존재하는지 확인하는 방법"
keywords: ["Java"]
---

{{< edit_this_page >}}

# 왜

디렉토리가 존재하는지 확인하는 것에 대해 이야기해보겠습니다. 프로그램을 작성하다보면 디렉토리의 존재 여부를 확인해야 할 때가 있기 때문입니다. 예를 들어 사용자가 업로드 한 파일이 저장될 디렉토리가 존재하는지 여부를 확인해야 할 수 있습니다.

# 어떻게

자바에서 디렉토리의 존재 여부를 확인하는 방법은 간단합니다. `File` 클래스의 `exists()` 메소드를 사용하면 됩니다. 코드 예시를 살펴보겠습니다.

```Java
import java.io.File;

public class CheckDirectory {
    public static void main(String[] args) {
        // 존재하는 디렉토리 경로
        String existingDirectoryPath = "C:/existingDirectory";
        File existingDirectory = new File(existingDirectoryPath);
        boolean isExistingDirectory = existingDirectory.exists();

        // 존재하지 않는 디렉토리 경로
        String nonExistingDirectoryPath = "C:/nonExistingDirectory";
        File nonExistingDirectory = new File(nonExistingDirectoryPath);
        boolean isNonExistingDirectory = nonExistingDirectory.exists();

        // 결과 출력
        System.out.println("존재하는 디렉토리 경로: " + existingDirectoryPath);
        System.out.println("존재 여부: " + isExistingDirectory);

        System.out.println("존재하지 않는 디렉토리 경로: " + nonExistingDirectoryPath);
        System.out.println("존재 여부: " + isNonExistingDirectory);
    }
}
```

위의 코드를 실행하면 다음과 같은 결과를 볼 수 있습니다.

```
존재하는 디렉토리 경로: C:/existingDirectory
존재 여부: true
존재하지 않는 디렉토리 경로: C:/nonExistingDirectory
존재 여부: false
```

# 딥 다이브

`exists()` 메소드는 디렉토리 뿐만 아니라 파일이나 심지어 디바이스도 존재 여부를 확인할 수 있습니다. 이 메소드는 파일이나 디렉토리의 실제 존재 여부를 알려주는 것이 아니라 해당 경로에 대한 파일 시스템 엔트리가 있는지 확인하는 것입니다.

또한, 이 메소드는 예외 처리를 문맥에 따라 다르게 처리할 수 있습니다. 예를 들어 파일이나 디렉토리가 존재하지 않는 경우 `SecurityException`을 던질 수도 있습니다. 이를 적절히 처리하는 것이 중요합니다.

# 참고

- [File 클래스 - Java API 문서](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java 예외 처리 - Java 을 배워보자!](https://wikidocs.net/229)