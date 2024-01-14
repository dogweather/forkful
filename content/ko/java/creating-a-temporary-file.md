---
title:    "Java: 일시적인 파일 생성하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 생성하는 것을 왜 해야 할까요? 자바 프로그래밍에서 임시 파일을 사용하는 이유는 다양합니다. 일시적으로 데이터를 저장하기 위해, 또는 프로그램 실행 중에 필요한 파일을 생성하기 위해 사용할 수 있습니다. 임시 파일을 생성하면 프로그래머가 파일 이름을 지정할 필요 없이 프로그램에서 생성된 임의의 파일명을 사용할 수 있어서 편리합니다.

## 생성 방법

자바에서 임시 파일을 생성하는 방법은 간단합니다. 먼저 `java.io.File` 클래스의 `createTempFile()` 메소드를 사용합니다. 이 메소드는 임시 파일을 생성하고 `File` 객체를 반환합니다. 아래는 이 메소드를 사용하는 예제 코드입니다.

```Java
import java.io.File;
import java.io.IOException;

public class TempFileExample {

    public static void main(String[] args) {

        // 임시 파일 생성
        File tempFile = null;
        try {
            // 첫 번째 매개변수는 접두사, 두 번째 매개변수는 확장자입니다.
            tempFile = File.createTempFile("myTempFile", ".txt");
        } catch (IOException e) {
            e.printStackTrace();
        }

        // 임시 파일의 경로 출력
        System.out.println("임시 파일 경로: " + tempFile.getPath());

        // 임시 파일 삭제
        tempFile.delete();

    }

}
```

위 코드를 실행하면 아래와 같은 출력이 나옵니다.

```
임시 파일 경로: /var/folders/6k/sprxnbnx3cn3lvbkky5fm3YIP8LR6u/T/myTempFile2897470459548927704.txt
```

보다시피 `createTempFile()` 메소드를 호출하면 임시 파일이 생성되고 해당 파일의 경로가 출력됩니다. 임시 파일을 사용한 후에는 `delete()` 메소드를 호출하여 파일을 삭제해야 합니다.

## 깊이 들어가기

실제로 자바에서 임시 파일은 어떻게 생성되는 걸까요? 자바에서는 임시 파일을 생성할 때 운영체제의 `tmp` 디렉토리에 생성합니다. 이때 `createTempFile()` 메소드는 자동으로 임시 파일의 이름을 생성합니다. 임시 파일의 이름은 `java.io.tmpdir` 시스템 프로퍼티와 일치하는 형식으로 생성됩니다. 운영체제마다 `tmp` 디렉토리의 위치는 다를 수 있기 때문에 `java.io.tmpdir` 시스템 프로퍼티를 사용하는 것이 안전합니다.

## See Also

- [Java Docs: createTempFile()](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)
- [Java File I/O](https://www.baeldung.com/java-io)
- [Java System Properties](https://www.geeksforgeeks.org/system-properties-java/#:~:text=System.-,Properties%20is%20a%20class%20provided%20by%20Java%20which%20gives%20the%20information%20about%20the%20current%20system.,-getAll()%20method"Java System Properties")