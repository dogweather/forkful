---
title:                "Java: 표준 오류에 글 쓰기"
simple_title:         "표준 오류에 글 쓰기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
표준 에러 출력에 참여하는 이유를 설명합니다.

일반적인 Java 프로그래머인 경우, 콘솔 출력을 사용하여 프로그램의 상태를 확인하고 디버깅하는 경우가 많습니다. 하지만 때로는 콘솔 출력만으로는 충분하지 않은 경우가 있습니다. 이때 표준 에러 출력을 사용하면 더 많은 정보를 얻을 수 있습니다.

## 어떻게
표준 에러 출력을 사용하는 방법을 예제 코드를 통해 알려드리겠습니다.

```Java
System.err.println("이것은 표준 에러 출력입니다.");
```

위의 코드를 실행하면 콘솔 출력은 `이것은 표준 에러 출력입니다.`라고 출력됩니다. 하지만 표준 에러 출력으로는 다른 정보를 얻을 수 있습니다. 예를 들어, 아래 코드를 실행하면 파일을 찾을 수 없는 에러가 발생하게 됩니다.

```java
File file = new File("이상한파일.txt");
file.getInputStream();
```

이때 표준 에러 출력을 추가해보면 `java.io.FileNotFoundException: 이상한파일.txt (지정된 파일을 찾을 수 없습니다)`와 같은 추가적인 정보를 얻을 수 있습니다.

```Java
File file = new File("이상한파일.txt");
try {
  file.getInputStream();
} catch (FileNotFoundException e) {
  System.err.println(e.toString());
}
```

표준 에러 출력을 추가함으로써 프로그램이 원인을 파악하고 해결하는데 도움이 됩니다.

## 깊이 파고들기
표준 에러 출력은 프로그램의 디버깅을 용이하게 해주는 도구입니다. 하지만 이외에도 다양한 용도로 사용될 수 있습니다.

예를 들어, 프로그램을 실행하는 과정에서 발생한 오류를 사용자에게 알려주는 경우가 있습니다. 이때 표준 에러 출력을 사용하면 오류 메시지를 콘솔에 출력함으로써 사용자에게 문제를 파악하고 해결할 수 있는 기회를 제공할 수 있습니다.

또한, 로깅 시스템에서도 표준 에러 출력을 사용하는 경우가 있습니다. 로그에 기록할 수 없는 에러가 발생했을 때 디버깅 용도로 표준 에러 출력을 사용하여 추가적인 정보를 얻을 수 있습니다.

## See Also
- [Java 표준 에러 출력 문서 (Oracle)](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
- [Java 에러 핸들링 방법 (Baeldung)](https://www.baeldung.com/java-error-handling)
- [Java 로깅 시스템 소개 (Techworld with Nana)](https://www.techworldwithnana.com/2018/11/17/5-ways-to-logging-in-java/)