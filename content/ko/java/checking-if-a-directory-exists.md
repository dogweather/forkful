---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:57:04.611136-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"

category:             "Java"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇을 하며 왜 하나요?)
디렉토리가 존재하는지 확인하는 것은 파일 시스템에 특정 경로가 존재하는지 아닌지 검사하는 과정입니다. 프로그래머들이 파일을 읽거나 쓸 때 또는 애플리케이션이 특정 작업을 수행하기 전에 이를 확인해 에러를 방지하고 불필요한 작업을 하지 않기 위해 사용합니다.

## How to (방법):
```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {

        String directoryPath = "/path/to/directory";
        
        boolean directoryExists = Files.exists(Paths.get(directoryPath));
        
        System.out.println("Does the directory exist? " + directoryExists);
    }
}
```
Sample Output:
```
Does the directory exist? true
```
또는 false가 출력될 수 있습니다. 이는 지정한 경로의 디렉토리의 존재 여부에 따라 달라집니다.

## Deep Dive (심층 분석):
과거에는 `File` 클래스의 `exists()` 메소드를 많이 사용했습니다. 하지만 Java 7부터는 NIO 패키지가 도입되었고 `Files`와 `Path` 클래스로 더 깔끔하고 유연한 파일 시스템 통제가 가능해졌습니다. 앞서 제시한 코드가 바로 그 예입니다. `Files.exists()`는 내부적으로 파일 시스템을 접근하여 경로의 존재를 확인합니다. 대안으로, 디렉토리가 확실히 존재하는지 더 철저하게 확인하고자 할 때 `Files.isDirectory()`도 사용할 수 있으며, `File` 대신 `Path`를 사용하는 것은 이후 파일 작업에 더 큰 유연성을 제공합니다.

## See Also (추가 자료):
- Java NIO 파일 API 공식 문서: [Oracle Docs](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
