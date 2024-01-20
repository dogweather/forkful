---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜 필요하고, 무엇인가?
임시 파일(temporary file)을 생성한다는 것은, 프로그램 실행 도중 일시적으로 데이터를 저장할 파일을 만드는 것입니다. 보통, 프로그래머들이 더 큰 파일을 처리하거나 데이터 정렬, 사용자 세션 상태 유지 등의 목적으로 임시 파일을 만듭니다.

## 어떻게 만들까?
자바를 사용하여 임시 파일을 생성하는 방법은 매우 간단합니다. `java.nio.file` 패키지의 `Files` 클래스로 손쉽게 만들 수 있습니다.

```Java
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            Path tempFile = Files.createTempFile("my-temp-file", ".txt");
            System.out.println("Temporary file created: " + tempFile.toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

이 프로그램을 실행하면, 다음과 같이 출력됩니다.

```Java
Temporary file created: /tmp/my-temp-file8341982734712839.txt
```

## 깊게 알아보기
* **역사적 맥락**: 임시 파일은 프로그래밍의 초기 시절부터 사용되었습니다. 컴퓨터에 저장 공간이 부족할 때, 임시 파일은 연산을 분할하여 데이터를 처리하는 데 유용했습니다.
* **대안들**: 많은 경우 메모리에 데이터를 저장하는 것이 효율적일 수 있습니다. 하지만 대용량 데이터 처리나 메모리 소모를 줄이기 위해서는 임시 파일이 유용합니다.
* **구현 세부사항**: `Files.createTempFile` 메서드는 현재 시스템의 임시 디렉토리에 파일을 생성합니다. 이 디렉토리는 시스템마다 다르지만, 많은 시스템에서는 '/tmp' 또는 'C:\Windows\Temp'경로를 사용합니다.

## 참고자료
* 자바 `Files` 클래스: [Oracle 공식 문서](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)
* Java I/O와 NIO 관련: [Jenkov's Tutorial](http://tutorials.jenkov.com/java-io/index.html)