---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:16.610967-07:00
description: "\uBC29\uBC95: #."
lastmod: '2024-03-13T22:44:55.076180-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:


### `java.nio.file` 사용하기 (표준 라이브러리)
자바의 새로운 I/O(NIO) 패키지(`java.nio.file`)는 파일을 다루는 데 더 다양한 접근 방식을 제공합니다. `Files.write()`를 사용하여 파일에 작성하는 간단한 방법은 다음과 같습니다:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("Line 1", "Line 2", "Line 3");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("File written successfully!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

출력:

```
File written successfully!
```

### `java.io` 사용하기 (표준 라이브러리)
보다 전통적인 접근법을 위해, `java.io.FileWriter`는 텍스트 파일을 간단하게 작성하기에 좋은 선택입니다:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("Hello, World!\n");
            writer.append("This is another line.");
            System.out.println("File written successfully!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

출력:

```
File written successfully!
```

### Apache Commons IO 사용하기
Apache Commons IO 라이브러리는 파일 작성을 포함한 많은 작업을 단순화합니다. 다음은 `FileUtils.writeStringToFile()`을 사용하여 파일에 작성하는 방법입니다:

먼저, 프로젝트에 의존성을 추가합니다. Maven을 사용하는 경우 포함시키세요:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- 최신 버전 확인 -->
</dependency>
```

그런 다음, 다음 코드를 사용하여 파일에 텍스트를 작성하세요:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "This is text written using Commons IO.", "UTF-8");
            System.out.println("File written successfully!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

출력:

```
File written successfully!
```
