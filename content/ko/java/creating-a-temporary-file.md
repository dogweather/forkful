---
title:    "Java: 임시 파일 만들기"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜

이번 글에서는 자바 프로그래밍에서 임시 파일을 생성하는 이유에 대해 알아보겠습니다. 임시 파일은 우리가 실제로 사용하는 파일이 아니지만, 일시적으로 필요한 정보를 담기 위해 사용됩니다. 보통은 프로그램의 실행 중에 생성되고, 사용이 끝나면 자동으로 삭제됩니다.

## 만드는 방법

우선 임시 파일을 생성하기 전에, `java.io.File` 패키지를 import 해주어야 합니다. 그리고 `createTempFile()` 메소드를 사용하여 임시 파일을 생성할 수 있습니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```Java
import java.io.File;

public class TempFileExample {
  public static void main(String[] args) {
    try {
      // 파일이 저장될 디렉토리와 파일 접두사를 지정합니다.
      File tempFile = File.createTempFile("myTempFile", ".txt");
      // 생성된 임시 파일의 경로를 출력합니다.
      System.out.println("임시 파일 경로: " + tempFile.getAbsolutePath());
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
```

위의 코드를 실행하면, 임시 파일 경로가 출력될 것입니다. 또한, 파일이 삭제되기 전까지는 해당 파일을 사용할 수 있습니다.

## 심화 학습

만약 임시 파일을 생성하는 과정에서 에러가 발생한다면 어떻게 해야 할까요? 이러한 경우에 대비하여 예외처리를 해주어야 합니다. 또한, `createTempFile()` 메소드를 사용할 때, 저장될 디렉토리와 파일 이름, 확장자를 지정해주어야 합니다. 그렇지 않으면, 운영체제가 임시 파일을 생성하기 위한 임시 디렉토리를 임의로 지정할 수 있기 때문입니다.

## 참고 자료

- [Java 공식 문서 - 임시 파일 생성](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)
- [자바의 지혜 - 임시 파일 생성하기](https://www.javajigi.net/2018/01/21/Java-Temp-File/)
- [namu.wiki - 자바 스터디 팁](https://namu.wiki/w/자바/스터디/팁)

## 더 알아보기

- [Java.io 패키지 API 문서](https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html)
- [Java 임시 파일 관리 방법](https://www.codejava.net/java-core/the-java-io-package/java-io-management-of-temporary-files)