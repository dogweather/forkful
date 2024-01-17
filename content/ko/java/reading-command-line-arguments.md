---
title:                "명령 줄 인수 읽기"
html_title:           "Java: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 뭐 & 왜?

명령 줄 인수를 읽는 것이 무엇인지와 프로그래머들이 왜 이를 하는지에 대해 설명합니다.

## 어떻게:

```java
public class CommandLineArguments {
    public static void main(String[] args) {
        for(int i = 0; i < args.length; i++) {
            System.out.println("인수 " + (i+1) + " : " + args[i]);
        }
    }
}
```

이 코드는 입력된 모든 인수를 차례대로 출력하는 간단한 Java 프로그램입니다. 예를 들어, 명령 줄에서 `java CommandLineArguments argument1 argument2 argument3`를 실행하면 다음과 같이 출력됩니다.

```
인수 1 : argument1
인수 2 : argument2
인수 3 : argument3
```

## 깊이 들어가보면:

명령 줄 인수를 읽는 것은 프로그래밍에서 자주 사용되는 기술입니다. 이 기술은 특정 프로그램을 실행할 때 프로그램에 필요한 값들을 외부에서 전달해주는 방법으로 사용됩니다. 예를 들어, 웹 서버를 구축할 때 사용되는 매개변수를 전달하는 데에도 명령 줄 인수를 사용할 수 있습니다. Java에서는 명령 줄 인수를 `args`라는 이름의 String 배열로 전달합니다. 이 배열을 사용하여 프로그램 내에서 인수들을 읽을 수 있습니다.

다른 대안으로는 프로그램 내에서 직접 값을 지정하는 방식이 있습니다. 하지만 이 경우에는 매번 프로그램을 수정해야 하기 때문에 유연성이 떨어집니다. 따라서 명령 줄 인수를 읽는 방식을 사용하는 것이 좋습니다.

## 참고 자료:

- [Command Line Arguments in Java](https://www.baeldung.com/java-command-line-arguments)
- [Java Command Line Arguments Example](https://javarevisited.blogspot.com/2012/10/parse-int-parseint-to-convert-string-to-integer-java-example-tutorial-program-code-example.html)