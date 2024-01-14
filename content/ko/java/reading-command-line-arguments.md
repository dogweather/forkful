---
title:    "Java: 컴퓨터 프로그래밍에서의 명령 줄 인수 읽기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 방법에 대해 궁금할 수 있습니다. 이 글을 통해 라인 인수를 읽는 이유에 대해 배워보세요!

## 어떻게

커맨드 라인 인수를 읽는 방법은 매우 간단합니다. 먼저, main 메소드의 매개변수로 String 배열을 사용합니다. 이 배열은 프로그램 실행 시 전달된 인수들을 담고 있습니다. 그리고 우리는 인수들을 반복문을 통해 하나씩 가져와서 필요한 작업을 수행할 수 있습니다. 아래의 예제 코드를 참고해보세요.

```Java
public static void main(String[] args) {
    for (String arg : args) {
        System.out.println(arg);
    }
}
```

만약 프로그램을 아래와 같은 커맨드로 실행한다면, "Hello"와 "World"라는 두 개의 인수가 출력될 것입니다.

```
java Program Hello World
```

## 깊이 파고들기

커맨드 라인 인수를 읽는 것은 프로그래밍에서 매우 유용한 기능입니다. 이를 통해 사용자로부터 입력을 받거나, 여러 가지 설정값을 지정할 수 있습니다. 또한 커맨드 라인 인수를 사용하면 프로그램을 실행할 때마다 다르게 동작하도록 만들 수 있습니다.

## 참고 자료

- [Java Docs: Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [GeeksforGeeks: Command Line arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
- [Baeldung: Guide to Java Command-Line arguments](https://www.baeldung.com/java-command-line-arguments)