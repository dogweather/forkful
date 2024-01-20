---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 파일을 읽는 것은 컴퓨터가 파일의 데이터를 직렬화된 형태로 인지하게 하는 과정입니다. 이는 개발자가 사용자 데이터를 저장하고 검색하거나, 외부 시스템과 상호작용하는 데 필수적인 기능입니다.

## 방법:

자바를 사용하여 텍스트 파일을 읽는 기본적인 방법은 `java.nio.file` 패키지의 `Files` 클래스와 `Paths` 클래스를 활용하는 것입니다:

```Java
import java.nio.file.*;

public class Main {
  public static void main(String[] args) {
    String path = "textfile.txt";
    try {
      String data = new String(Files.readAllBytes(Paths.get(path)));
      System.out.println(data);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
```

이 코드는 `textfile.txt`라는 파일의 내용을 읽고 출력합니다.

## 깊이 조사: 

텍스트 파일 읽기는 컴퓨터 분야의 선구자들이 개발한 가장 기본적인 I/O 작업 중 하나입니다. 오늘날에는 다양한 방법으로 텍스트 파일을 읽을 수 있습니다: 스트림 API, 스캐너 등 다양한 자바 클래스와 라이브러리가 있습니다. 무엇을 선택할지는 요구사항과 선호에 따라 달라집니다.

`Files` 클래스를 통해 파일을 읽는 것은 한 번에 전체 파일을 메모리에 로드합니다. 그래서 큰 파일을 처리할 때는 메모리 문제가 발생할 수 있습니다. 이런 경우, `java.util.Scanner` 나 `java.io.BufferedReader` 등을 사용하여 파일을 줄 단위로 읽는 것이 더 적합합니다.

## 참고 자료:

- Java의 I/O API에 관련하여 Oracle 공식 문서: https://docs.oracle.com/javase/tutorial/essential/io/index.html
- StackOverflow에서 'Java로 텍스트 파일 읽기'에 관한 질문과 답변: https://stackoverflow.com/questions/4716503/reading-a-plain-text-file-in-java