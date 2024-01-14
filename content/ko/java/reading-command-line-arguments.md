---
title:                "Java: 컴퓨터 프로그래밍의 커맨드 라인 인자 읽기"
simple_title:         "컴퓨터 프로그래밍의 커맨드 라인 인자 읽기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 방법을 배우는 것은 프로그래밍을 더 효율적으로 할 수 있게 해 줍니다. 사용자의 입력에 따라 다른 결과를 출력하도록 코드를 작성할 수 있으며, 유연성과 사용자 친화적인 프로그램을 만들 수 있습니다.

## 가는 방법

커맨드 라인 인수를 읽는 가장 간단한 방법은 Java 프로그램의 main 메소드에 전달되는 인수를 읽는 것입니다. 이를 위해 main 메소드의 매개변수로 String 배열을 사용할 수 있습니다. 다음은 간단한 예제 코드입니다.

```Java
public static void main(String[] args){
    // 첫 번째 인수 출력
    System.out.println("첫 번째 인수: " + args[0]);
    // 두 번째 인수 출력
    System.out.println("두 번째 인수: " + args[1]);
}
```

위의 코드를 실행하기 위해서는 커맨드 라인에서 다음과 같이 프로그램을 실행해야 합니다.

```
java main.java 첫번째인수 두번째인수
```

위의 예제에서는 두 개의 인수를 받지만, 필요에 따라 더 많은 인수를 받을 수도 있습니다. 또한 인수말고도 다른 커맨드 라인 정보를 읽는 방법도 있습니다. 이에 대해 더 알아보려면 아래의 "더 깊게 들어가기" 섹션을 참고하세요.

## 더 깊게 들어가기

커맨드 라인 인수를 읽는 것 이외에도, Java에는 인수를 처리하는 다양한 방법이 있습니다. 예를 들어 Scanner 클래스를 사용하면 표준 입력에서 입력을 받을 수 있고, Properties 클래스를 사용하면 프로퍼티 파일에서 값을 읽어올 수 있습니다. 또한, Apache Commons CLI 라이브러리를 이용하면 다양한 옵션을 지정하여 프로그램을 실행할 수 있습니다.

더 많은 예제와 자세한 정보를 원한다면, 공식 Java 문서를 참고하시기 바랍니다.

## 이것도 참고하세요

- [Java 공식 문서](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html[]()miscellaneous)