---
date: 2024-01-26 01:39:51.893273-07:00
description: "\uB9AC\uD329\uD130\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\
  \uB4DC\uC758 \uAD6C\uC870\uB97C \uC7AC\uC870\uC815\uD558\uB294 \uACFC\uC815\u2014\
  \uADF8 \uC694\uC778\uC744 \uBCC0\uACBD\uD558\uB294 \uAC83\u2014\uC774\uC9C0\uB9CC\
  \ \uADF8 \uC678\uBD80 \uB3D9\uC791\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC2B5\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC18C\uD504\uD2B8\uC6E8\uC5B4\
  \uC758 \uBE44\uAE30\uB2A5\uC801 \uC18D\uC131\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\
  \uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4. \uC774\uB97C \uD1B5\
  \uD574 \uAC00\uB3C5\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uACE0, \uBCF5\uC7A1\uC131\
  \uC744 \uC904\uC774\uBA70, \uCF54\uB4DC\uB97C \uD5A5\uD6C4 \uC0AC\uC5C5\uC5D0 \uB354\
  \u2026"
lastmod: '2024-03-13T22:44:55.061109-06:00'
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD130\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\uB4DC\
  \uC758 \uAD6C\uC870\uB97C \uC7AC\uC870\uC815\uD558\uB294 \uACFC\uC815\u2014\uADF8\
  \ \uC694\uC778\uC744 \uBCC0\uACBD\uD558\uB294 \uAC83\u2014\uC774\uC9C0\uB9CC \uADF8\
  \ \uC678\uBD80 \uB3D9\uC791\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC18C\uD504\uD2B8\uC6E8\uC5B4\uC758\
  \ \uBE44\uAE30\uB2A5\uC801 \uC18D\uC131\uC744 \uAC1C\uC120\uD558\uAE30 \uC704\uD574\
  \ \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4. \uC774\uB97C \uD1B5\uD574\
  \ \uAC00\uB3C5\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uACE0, \uBCF5\uC7A1\uC131\uC744\
  \ \uC904\uC774\uBA70, \uCF54\uB4DC\uB97C \uD5A5\uD6C4 \uC0AC\uC5C5\uC5D0 \uB354\u2026"
title: "\uB9AC\uD329\uD130\uB9C1"
weight: 19
---

## 무엇 & 왜?
리팩터링은 기존 컴퓨터 코드의 구조를 재조정하는 과정—그 요인을 변경하는 것—이지만 그 외부 동작은 변경하지 않습니다. 프로그래머들은 소프트웨어의 비기능적 속성을 개선하기 위해 이 작업을 수행합니다. 이를 통해 가독성을 향상시키고, 복잡성을 줄이며, 코드를 향후 사업에 더 유지보수하기 쉽게 만듭니다.

## 방법:
조직이나 명확성이 떨어져 리팩터링이 필요한 간단한 Java 클래스를 살펴봅시다.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // 기타 연산들...
    }
}
```

리팩터링 후에는 다음과 같습니다:

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // 기타 연산들...
}
```

리팩터링을 통해 메서드 이름과 매개변수를 가독성 좋게 개선했고, 단일 메서드 내에서 조건 분기의 필요성을 제거했습니다. 각 연산은 이제 그 목적을 명확하게 설명합니다.

## 심층 탐구:
리팩터링은 코드 가독성과 객체 지향 설계에 중점을 둔 Smalltalk 커뮤니티에서 그 뿌리를 찾을 수 있지만, Martin Fowler의 주옥같은 책 "Refactoring: Improving the Design of Existing Code"의 출판 이후, 특히 90년대 말과 00년대 초반 Java 세계에서 정말로 주목받기 시작했습니다.

리팩터링의 대안으로는 코드를 처음부터 다시 작성하는 것이 있지만, 기능성을 방해하지 않는 점진적 변경을 포함한다는 점에서 리팩터링이 종종 선호됩니다.

Java(또는 모든 프로그래밍 언어)에서 리팩터링을 구현할 때의 세부 사항은 코드 냄새—코드에서 더 심각한 문제를 지시하는 지표—를 이해하는 데 중심을 둡니다. 긴 메서드, 큰 클래스, 중복 코드, 과도한 원시 사용과 같은 냄새가 있습니다. Extract Method, Move Method 또는 Replace Temp with Query와 같은 리팩터링 패턴을 적용함으로써 개발자는 이러한 냄새를 체계적으로 다루면서 언제나 코드가 기능적으로 유지되도록 할 수 있습니다.

IntelliJ IDEA의 리팩터링 지원이나 Eclipse용 플러그인 같은 자동화 도구는 변수, 메서드 및 클래스의 이름 변경, 메서드 또는 변수의 추출, 그리고 메서드 또는 클래스를 다른 패키지나 네임스페이스로 이동하는 것과 같은 리팩터링을 자동화하는 데 도움을 줄 수 있습니다.

## 참고 자료:
- Martin Fowler의 "Refactoring: Improving the Design of Existing Code": https://martinfowler.com/books/refactoring.html
- Refactoring.Guru에서의 리팩터링 기법: https://refactoring.guru/refactoring/techniques
- Eclipse에서의 자동 리팩터링: https://www.eclipse.org/eclipse/news/4.18/jdt.php
- IntelliJ IDEA의 리팩터링 기능: https://www.jetbrains.com/idea/features/refactoring.html

이러한 각 자료들은 리팩터링의 원칙을 이해하거나 이 원칙들을 실제로 적용하는 데 활용될 수 있는 도구를 제공합니다.
