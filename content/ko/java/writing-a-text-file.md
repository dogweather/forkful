---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? 
(무엇과 왜?)
텍스트 파일 쓰기란 문자 데이터를 파일 형태로 디스크에 저장하는 것입니다. 프로그래머들은 데이터 이동, 로깅, 설정 저장 등의 이유로 이를 수행합니다.

## How to:
(방법:)
```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteTextFile {
    public static void main(String[] args) {
        String path = "example.txt";
        String content = "Hello, this is a text file.";

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(path))) {
            writer.write(content);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
example.txt 파일의 샘플 출력:
```
Hello, this is a text file.
```

## Deep Dive
(심층 탐구)
과거에는 java.io 패키지를 사용하여 파일에 데이터를 쓰곤 했습니다. java.nio 패키지가 나오며 향상된 입출력(I/O) 기능을 이용할 수 있게 되었습니다. java.nio.file.Files 클래스의 write 메서드는 파일 쓰기를 한 줄로 간단히 처리할 수 있으며, 대량의 데이터 처리에 더 효율적입니다. 스트림을 사용하면 파일 쓰기가 중단될 때 자동으로 리소스를 해제합니다.

## See Also
(참고 자료)
- Oracle Java Documentation: https://docs.oracle.com/en/java/
- Java NIO: https://docs.oracle.com/javase/8/docs/api/java/nio/package-summary.html
- BufferedWriter: https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html