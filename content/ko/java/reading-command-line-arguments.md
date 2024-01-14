---
title:                "Java: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인자를 읽는 방법에 대해 배우기 전에, 커맨드 라인 인자를 읽는 이유는 무엇일까요? 자바 프로그래밍에서 커맨드 라인 인자는 매우 중요한 역할을 하며, 프로그램을 실행할 때 유저가 입력한 정보를 전달하는 역할을 합니다. 따라서 이 기능을 제대로 이해하고 사용하는 것은 중요합니다.

## 어떻게

커맨드 라인 인자를 읽는 방법은 간단합니다. 우선 `public static void main(String[] args)` 메소드 안에 있는 `String[] args` 파라미터는 커맨드 라인 인자를 저장하는 배열입니다. 이 배열을 `for` 루프나 인덱스를 이용하여 접근할 수 있습니다. 예를 들어서, 만약 유저가 `java Program.java hello 123`라는 명령어를 입력했다면, `args[0]`에는 `"hello"`가, `args[1]`에는 `"123"`이 저장됩니다.

```Java
public class Program {
    public static void main(String[] args) {
        for (String arg : args) {
            System.out.println(arg);
        }
    }
}
```

위의 코드를 실행하면, 다음과 같은 결과를 볼 수 있습니다.

```
hello
123
```

## 상세히 알아보기

더 깊이 있는 정보를 알고 싶다면, 커맨드 라인 인자를 읽는 방법에 대해 더 많이 알아보아야 합니다. 예를 들어, `java Program.java -h`라는 명령어를 입력하면, `args[0]`에는 `"-h"`가 저장되는 것이 아니라, `args[0]`에는 `"-h"`와 같은 옵션을 구분하는 데 사용되는 특별한 문자(`'-'`)가 저장됩니다. 또한 `args[0]`에만 해당되는 것이 아니고, 모든 커맨드 라인 인자에 적용됩니다.

## 더 읽어보기

깊이 있는 정보를 알기 위해 다른 링크들을 참조해 보세요.

[Java의 커맨드 라인 인자 확인하기](https://www.baeldung.com/java-command-line-arguments)

[자바 커맨드 라인 인자에 대해 더 알아보기](https://www.lifewire.com/passing-command-line-arguments-2034314)

[심화 자바 프로그래밍: 커맨드 라인 인자 읽기](https://www.geeksforgeeks.org/command-line-arguments-in-java/)

## 더 알아보기