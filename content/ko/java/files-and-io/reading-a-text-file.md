---
date: 2024-01-20 17:54:29.515652-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Sample output for both\
  \ examples."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.836744-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Sample output for both examples."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

## How to: (어떻게 하나요?)
```java
import java.nio.file.*;
import java.io.*;
import java.util.stream.*;

public class TextFileReader {
    public static void main(String[] args) {
        Path filePath = Paths.get("example.txt");

        // Old-school try-catch block
        try (BufferedReader reader = Files.newBufferedReader(filePath)) {
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        // Modern Java with Streams
        try (Stream<String> lines = Files.lines(filePath)) {
            lines.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Sample output for both examples:
```
Hello, this is a line of text.
And here's another line.
```

## Deep Dive (깊이 파보기)
옛날에는 파일을 읽으려면 `FileReader`와 `BufferedReader`를 사용하는 복잡한 코드를 써야 했습니다. `Scanner` 클래스도 있었지만, 큰 파일을 처리하는 데는 적합하지 않았죠.  Java 7부터는 NIO 패키지가 도입되어 `Files`와 `Paths`를 사용하면 쉽고 현대적인 방식으로 파일을 읽을 수 있습니다.

파일 읽기에는 여러 방법이 있긴 하지만, 상황에 따라 적절한 도구를 선택하는 것이 좋습니다. 예를 들어, 큰 파일을 읽을 때는 스트림을 사용하면 메모리를 절약할 수 있지만, 간단한 구성 파일이라면 `Files.readAllLines`처럼 한 번에 모든 줄을 읽어 올 수 있는 방법이 더 쉬울 거예요.

Java에서 파일을 읽는 방식은 I/O 작업의 예외 처리를 항상 염두에 두어야 합니다. 예외 처리가 잘못되면 데이터 손실이나 보안 취약점과 같은 심각한 문제가 발생할 수 있습니다.

## See Also (더 보기)
- [Oracle Java Documentation - Reading, Writing, and Creating Files](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html)
- [Baeldung - Reading a File into a String in Java](https://www.baeldung.com/java-read-file)
- [Stack Overflow - How do I create a Java string from the contents of a file?](https://stackoverflow.com/questions/326390/how-do-i-create-a-java-string-from-the-contents-of-a-file)
