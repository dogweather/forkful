---
title:    "Java: 명령줄 인수 읽는 방법"
keywords: ["Java"]
---

{{< edit_this_page >}}

## 왜
이유: 왜 누군가가 명령 줄 인수를 읽는 것에 참여해야 하는지에 대해 1-2 문장으로 설명합니다.

명령 줄 인수를 읽는 것은 Java 프로그래밍에서 매우 유용한 기능입니다. 이를 통해 사용자가 프로그램을 실행할 때 추가적인 정보를 입력할 수 있고, 이를 통해 프로그램 실행 결과를 조작할 수 있습니다. 또한, 명령 줄 인수를 통해 여러 개의 프로그램 인스턴스를 실행할 수 있으며, 이를 통해 효율적인 프로그램 관리가 가능해집니다.

## 사용 방법
```Java
public class CommandLineArgs {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("입력한 명령 줄 인수는 다음과 같습니다:");
            // for문을 사용하여 입력한 인수를 모두 출력합니다.
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("명령 줄 인수가 없습니다.");
        }
    }
}
```
위의 코드는 명령 줄 인수를 받아서 출력하는 간단한 예제입니다. ```main()``` 메서드의 파라미터 ```args```는 String 배열로써, 사용자가 입력한 명령 줄 인수들이 저장됩니다. 위의 예제에서는 for문을 사용하여 각 인수를 하나씩 출력하였습니다.

만약 위의 예제 코드를 ```CommandLineArgs.java```라는 파일로 저장하고, 이를 컴파일한 후 명령 줄에서 ```java CommandLineArgs hello world!```를 입력하면 다음과 같은 결과가 출력됩니다.
```
입력한 명령 줄 인수는 다음과 같습니다:
hello
world!
```

## 깊이 들어가기
명령 줄 인수를 읽는 과정은 매우 간단합니다. 사용자가 입력한 인수는 String 배열로 저장되기 때문에, 이를 반복문 등을 사용하여 모두 읽고 처리할 수 있습니다. 또한, 프로그래머가 원하는 형태로 입력한 인수를 처리할 수 있기 때문에 매우 유연하게 사용할 수 있습니다.

또한, 명령 줄 인수에는 옵션을 추가할 수도 있습니다. 예를 들어, 사용자가 프로그램의 동작을 조절하는 옵션을 입력할 수 있으며, 이를 통해 프로그램의 동작을 더욱 유연하게 만들 수 있습니다. 또한, 여러 개의 프로그램 인스턴스를 실행하고 명령 줄 인수를 통해 서로 다른 입력값을 전달할 수 있기 때문에, 대규모 프로그램 관리에 매우 유용한 기능입니다.

## 더 알아보기

### [Java Tutorials: Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
위에서 설명한 내용을 보다 자세하게 다루고 있는 Java 공식 문서입니다.

### [Baeldung: Command-Line Arguments in Java](https://www.baeldung.com/java-command-line-arguments)
명령 줄 인수를 다루는 여러 가지 기법들을 소개하고 있는 자세한 블로그 포스트입니다.

### [GeeksforGeeks: Command line arguments in Java](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
Java에서 명령 줄 인수를 다루는 방법을 코드 예제와 함께 상세히 설명하고 있는 블로그 포스트입니다.