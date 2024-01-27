---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
표준 오류에 기록하기는 프로그램 실행 중 오류 메시지를 별도로 출력하는 것입니다. 개발자는 일반 출력과 구분해서 문제를 빠르게 발견하고, 로깅과 디버깅을 용이하게 하기 위해 이 방법을 사용합니다.

## How to: (어떻게 하나?)
```java
public class StdErrExample {
    public static void main(String[] args) {
        // 정상 출력
        System.out.println("이것은 표준 출력입니다.");

        // 오류 출력
        System.err.println("이것은 에러 메시지입니다.");
    }
}
```
실행 결과:
```
이것은 표준 출력입니다.
이것은 에러 메시지입니다.
```
참고: 오류 메시지는 종종 붉은색으로 표시되지만, 이는 콘솔 설정에 따라 달라집니다.

## Deep Dive: (심층 분석)
표준 오류(stream)는 UNIX 시스템의 초기 설계에서 유래되었으며, 일반적으로 파일 디스크립터 2번에 해당합니다. 표준 출력(`System.out`)과는 대비되는 개념입니다. 대체 방법으로는 로깅 프레임워크를 사용하는 것이 있으며, 이는 더 정교하고 유연한 오류 관리를 가능하게 합니다. `System.err`의 내부 구현은 `PrintStream` 클래스에 기반하여, 자동으로 `flush`가 됩니다.

## See Also: (추가 자료)
- [System (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [PrintStream (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [Logging in Java with SLF4J](https://www.slf4j.org/manual.html)
