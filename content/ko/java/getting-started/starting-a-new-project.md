---
date: 2024-01-20 18:03:43.102396-07:00
description: "How to (\uC5B4\uB5BB\uAC8C): \uC27D\uAC8C \uB9D0\uD574 Java \uD504\uB85C\
  \uC81D\uD2B8\uB97C \uC2DC\uC791\uD558\uB294 \uAC74 \uAC04\uB2E8\uD574\uC694. IDE(\uD1B5\
  \uD569 \uAC1C\uBC1C \uD658\uACBD)\uB97C \uC4F0\uB358, \uC9C1\uC811 \uB514\uB809\uD1A0\
  \uB9AC\uB97C \uC124\uC815\uD558\uB358, \uAE30\uBCF8 \uAD6C\uC870\uB294 \uBE44\uC2B7\
  \uD569\uB2C8\uB2E4. \uC5EC\uAE30 IntelliJ IDEA\uB97C \uC0AC\uC6A9\uD558\uB294 \uAE30\
  \uBCF8 \uC608\uC81C\uB97C \uBCF4\uC5EC\uB4DC\uB9B4\uAC8C\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.049564-06:00'
model: gpt-4-1106-preview
summary: "\uC27D\uAC8C \uB9D0\uD574 Java \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\
  \uD558\uB294 \uAC74 \uAC04\uB2E8\uD574\uC694."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## How to (어떻게):
쉽게 말해 Java 프로젝트를 시작하는 건 간단해요. IDE(통합 개발 환경)를 쓰던, 직접 디렉토리를 설정하던, 기본 구조는 비슷합니다. 여기 IntelliJ IDEA를 사용하는 기본 예제를 보여드릴게요.

```java
// Main.java
public class Main {
    public static void main(String[] args) {
        System.out.println("새 프로젝트, 시작!");
    }
}
```

실행 결과:

```java
새 프로젝트, 시작!
```

## Deep Dive (심층 탐구):
과거엔 모든 것을 수동으로 설정해야 했어요. 디렉토리 생성, CLASSPATH 설정, 컴파일 방법 등을 직접 했죠. 지금은 IntelliJ, Eclipse 등의 IDE가 자동으로 처리해줘요. 더 나아가, Maven이나 Gradle 같은 빌드 도구를 사용해 종속성 관리와 빌드 과정을 자동화할 수 있어요.

단순 Java 프로젝트 외에도 스프링 부트나 마이크로 서비스를 위한 프로젝트를 시작하는 경우가 많아요. 이런 프로젝트들은 초기 설정에 더 많은 관심이 필요하지만, spring initializr(https://start.spring.io/) 같은 도구를 쓰면 쉽게 시작할 수 있습니다.

한편, 배운 것을 시험하고 싶을 땐, 코딩 도전 과제나 작은 클론 프로젝트부터 시작하는 것도 좋은 방법입니다.

## See Also (참고 자료):
- Oracle의 Java Tutorial: https://docs.oracle.com/javase/tutorial/
- IntelliJ IDEA 가이드: https://www.jetbrains.com/idea/guide/
- Maven: https://maven.apache.org/
- Gradle: https://gradle.org/
- Spring Initializr: https://start.spring.io/
