---
title:                "Java: 표준 오류에 쓰기"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜 *표준 에러*에 쓰레기를 쓰는가

표준 에러는 많은 프로그래밍 언어에서 사용되는 역할입니다. 이 글에서는 자바에서의 표준 에러 쓰기의 이유에 대해 알아보겠습니다. 

## 어떻게 쓰레기를 쓰나

아래의 코드 블록에서는 자바에서 표준 에러를 쓰는 방법을 살펴보겠습니다. 

```Java
public static void main(String[] args) {
    System.err.println("표준 에러 쓰기 예제");
}
```

위 코드를 실행하면 다음과 같은 결과를 볼 수 있습니다:

`표준 에러 쓰기 예제`

## 심층 탐구

자바에서 표준 에러를 쓰는 이유는 주로 디버깅을 위해서입니다. 보통 프로그램에서 발생하는 모든 오류와 예외를 표준 에러에 쓰레기하면서 프로그램이 실행되는 동안 오류를 실시간으로 알 수 있게 됩니다. 이를 통해 개발자는 빠르게 오류를 발견하고 수정할 수 있습니다. 

## 더 많은 정보

- [Java 표준 출력 vs. 표준 에러: 무엇이 다른가?](https://www.baeldung.com/java-standard-output-error)
- [Java에서 로그 작성하기: Log4j를 사용하는 방법](https://www.inflearn.com/course/%EC%9E%90%EB%B0%94-%EC%95%B1%EC%84%9C%EB%A6%AC%EB%B6%80-log4j-daily)
- [Java 오류 처리: 각각의 예외에 대한 다른 처리 방법](https://howtoprogram.xyz/2016/08/17/java-exception-handling/)

# 참고자료

- [Java 표준 입력과 표준 출력을 다루는 방법](https://javatutorial.net/java-syst