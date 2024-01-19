---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 필요한가?

디렉토리 존재 유무를 확인하는 것은 코드에서 특정 디렉토리가 이미 있는지 여부를 결정하는 일입니다. 프로그래머들이 이것을 필요로 하는 이유는 예기치 않은 오류를 방지하고 파일 작업 프로세스를 제어하기 위해서입니다.

## 어떻게 하는가:

Java의 `java.nio.file` 패키지를 사용하여 디렉토리가 존재하는지 확인할 수 있습니다. 아래는 기본 샘플 코드입니다.

```Java
import java.nio.file.*;

public class Main {
    public static void main(String[] args) {
        Path path = Paths.get("C:/example");

        if (Files.exists(path)) {
            System.out.println("Directory exists");
        } else {
            System.out.println("Directory doesn't exist");
        }
    }
}
```

이 코드를 실행하면 "C:/example" 디렉토리가 존재하는지에 따라 적절한 메시지가 출력됩니다.

## 깊은 이해:

디렉토리 존재 확인은 파일 시스템 작업의 초기단계에서 자주 사용되며, Java에서는 파일 API가 발전하면서 다양한 방법으로 사용할 수 있게 되었습니다. 이전에는 `java.io.File` 클래스를 사용했지만, Java 7부터 `java.nio.file.Files` 클래스가 등장하면서 더욱 효과적인 방법으로 디렉토리 확인을 할 수 있게 되었습니다.

다른 방법으로는 Apache Commons IO 라이브러리를 사용하는 것도 있습니다. 이 라이브러리를 사용하면 디렉토리 존재 확인 외에도 다른 일반적인 파일 작업을 수행할 수 있습니다.

그러나 대부분의 경우, `java.nio.file.Files` 클래스만으로 충분하며 프로그램의 성능과 효율성을 위해 추천됩니다.

## 참고 자료:

1. Java 공식 문서: [java.nio.file 패키지](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html)
2. [Apache Commons IO 라이브러리](https://commons.apache.org/proper/commons-io/)
3. [A Guide to java.nio.file.Path](https://www.baeldung.com/java-nio-path) at Baeldung