---
title:                "Java: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것에 대해 궁금해하는 분들이 많을 것 같아 이 글을 쓰게 되었습니다. 텍스트 파일을 읽는 방법과 그 과정에 대해 알아보고 실제로 코드를 작성해보며 자세히 살펴보겠습니다.

## 어떻게

먼저, 텍스트 파일을 읽기 위해서는 Java의 내장 라이브러리인 `java.io` 패키지를 사용해야 합니다. 다음 코드를 참고해보세요.

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadTextFile{
  public static void main(String[] args){
    // 파일 경로 지정
    String filePath = "sample.txt";

    // try-catch 문으로 예외 처리
    try{
      // BufferedReader를 이용해 파일을 읽음
      BufferedReader br = new BufferedReader(new FileReader(filePath));
      // 한 줄씩 읽어와서 출력
      String line;
      while ((line = br.readLine()) != null){
        System.out.println(line);
      }
      // 파일 닫기
      br.close();
    } catch (IOException e) {
       e.printStackTrace();
    }
  }
}
```

위 코드를 실행하면 `sample.txt` 파일 안의 내용이 한 줄씩 출력될 것입니다. `BufferedReader`는 `IOException`이 발생할 수 있기 때문에 `try-catch` 문으로 예외를 처리해주는 것이 중요합니다. 또한, 파일을 다 사용한 뒤에는 꼭 `close()` 메소드를 호출하여 자원을 반환해야 합니다.

## 깊이 들어가기

실제로 텍스트 파일을 읽는 과정은 그리 복잡하지 않습니다. `BufferedReader`를 이용해 파일을 읽기만 하면 됩니다. 하지만, 자주 사용하는 메소드들에 대해 더 자세히 알아보고 싶은 분들을 위해 간단히 설명하겠습니다.

- `BufferedReader.readLine()`: 한 줄씩 읽어오는 메소드로, 파일의 끝까지 읽으면 `null`을 반환합니다.

- `BufferedReader.skip(long n)`: `n` 바이트만큼 읽어와서 버립니다. 특정 부분을 건너뛰고 싶을 때 사용할 수 있습니다.

- `BufferedReader.mark(int readAheadLimit)`: 읽은 바이트의 위치를 표시합니다. `reset()` 메소드를 이용해 해당 위치로 다시 돌아갈 수 있습니다.

- `BufferedReader.reset()`: `mark()` 메소드를 이용한 표식 위치로 돌아갑니다.

자세한 내용은 공식 문서를 참고해주시면 됩니다.

## 관련 링크

- [Java 공식 문서 - Text Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [BufferedReader 문서](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Java IO Tutorial from Baeldung](https://www.baeldung.com/java-io-tutorial)