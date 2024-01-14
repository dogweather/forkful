---
title:                "Java: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜

임시 파일을 생성하는 것에 참여해야하는 사람들을 이해하는 것은 중요합니다. 일시적으로 생성 된 파일은 프로그램이 실행되는 동안 데이터를 저장하거나 처리하는 데 사용될 수 있습니다. 또한 임시 파일은 시스템 상태를 진단하고 디버깅하는 데에도 유용합니다.

# 어떻게

임시 파일을 생성하는 방법은 간단합니다. 먼저, `java.io.File` 클래스의 `createTempFile()` 메소드를 사용하여 임시 파일을 생성하고 파일 이름과 접두사를 제공합니다. 그리고나서 `createNewFile()` 메서드를 사용하여 새로운 파일을 생성합니다. 아래의 예제를 참조하세요.

```Java
import java.io.*;

public class CreateTempFileExample {
  public static void main(String[] args) {
    // 임시 파일을 저장할 디렉토리 경로
    String tempDir = System.getProperty("java.io.tmpdir");
    
    // 임시 파일 이름과 접두사
    String fileName = "tempfile";
    String prefix = "example";
    
    // 임시 파일 생성
    File tempFile = null;
    try {
      tempFile = File.createTempFile(fileName, prefix, new File(tempDir));
      // 파일 생성 여부 확인
      if (tempFile.createNewFile()) {
        System.out.println("임시 파일 " + tempFile.getName() + "을(를) 성공적으로 생성했습니다.");
      } else {
        System.out.println("임시 파일 생성에 실패했습니다.");
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
```

위의 예제 코드를 실행하면 아래와 같은 결과가 출력됩니다.

```
임시 파일 example6591025077057023345.tmp을(를) 성공적으로 생성했습니다.
```

# 딥 다이브

임시 파일을 생성하는 메소드는 다음과 같은 옵션을 제공합니다.

- 두 번째 매개 변수는 임시 파일의 경로를 지정할 수 있습니다.
- 세 번째 매개 변수를 사용하여 아무것도 전달하지 않으면 OS 기본 임시 디렉토리에 임시 파일이 생성됩니다.
- `createTempFile()` 메서드는 파일 이름이 5자리 숫자로 끝나는 임시 파일을 생성합니다. 파일 이름에 숫자가 적은 이유는 생성되는 임시 파일의 숫자가 고유한 것을 보장하기 위해서입니다.

위의 예제에서는 임시 파일 이름을 "tempfile"로 했습니다. 임시 파일 이름에 디렉토리 경로를 포함하려면 경로와 파일 이름을 `File` 클래스의 생성자에 전달하면 됩니다.

# 참고

- [Java File class documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java IO Tutorial](https://www.tutorialspoint.com/java/io/index.htm)
- [Creating temporary files in Java](https://www.baeldung.com/java-temporary-files)