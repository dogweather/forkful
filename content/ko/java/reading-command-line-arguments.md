---
title:                "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
html_title:           "Java: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜?

커맨드 라인 인자를 읽는 것이 왜 중요한지 궁금하신가요? 자바 프로그래밍에서 커맨드 라인 인자는 사용자로부터 입력값을 받는 가장 간단하고 빠른 방법입니다. 따라서 더욱 쉽고 효율적인 프로그램을 만들기 위해 커맨드 라인 인자를 읽을 줄 알아야 합니다.

## 방법

커맨드 라인 인자를 읽는 방법을 배우기 전에 먼저 `String[] args` 라는 매개변수가 있다는 것을 기억하세요. 이 매개변수는 `main` 메소드의 일부로 자동으로 제공되며 사용자로부터 입력받은 모든 인자를 담고 있습니다. 이제 실제로 어떻게 인자를 읽는지 살펴봅시다.

### 예제 1:
```java
public static void main(String[] args) {
  // 첫번째 인자 출력
  System.out.println(args[0]);
}
```

이 예제에서는 `args` 배열의 첫번째 요소, 즉 첫번째 커맨드 라인 인자를 출력합니다. 만약 프로그램을 다음과 같이 실행한다면 `hello`라는 문자열이 출력될 것입니다.

```bash
java MyProgram hello
```

### 예제 2:
```java
public static void main(String[] args) {
  // 마지막 인자 출력
  System.out.println(args[args.length-1]);
}
```

이번에는 `args` 배열의 길이를 이용하여 마지막 커맨드 라인 인자를 출력하는 예제입니다. 만약 프로그램을 다음과 같이 실행한다면 `bye`라는 문자열이 출력될 것입니다.

```bash
java MyProgram hello hi bye
```

## 깊이 파고들기

커맨드 라인 인자를 읽는 것은 프로그래밍에서 중요한 기술 중 하나입니다. 자바에서는 `String[] args`를 통해 커맨드 라인 인자를 쉽게 읽을 수 있지만, 실제로는 다양한 방법으로 인자를 처리할 수 있습니다. 예를 들어, `parseInt()` 메소드를 사용하여 숫자로 변환하는 방법도 있고, `switch` 문을 이용하여 다양한 인자에 따라 다른 작업을 수행하는 방법도 있습니다. 각각의 방법을 습득하면 더욱 다양하고 유연한 프로그래밍을 할 수 있을 것입니다.

## 관련 자료

- [Oracle Java Documentation](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Baeldung - Reading Command Line Arguments in Java](https://www.baeldung.com/java-command-line-arguments)
- [GeeksforGeeks - Command Line Argument in Java](https://www.geeksforgeeks.org/command-line-argument-in-java/)