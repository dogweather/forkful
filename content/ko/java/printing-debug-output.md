---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

Debug 출력은 프로그램이 실행되는 동안 핵심 정보를 표시하는 방법입니다. 이것은 코드를 디버깅하고 문제를 빠르게 예측하고 해결하는 데 도움이 됩니다.

## 어떻게 하는가:

단순한 경우, Java에서는 System.out.println()을 사용하여 디버그 출력을 프린트 할 수 있습니다.

```Java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

이 코드의 실행으로, 출력은 다음과 같습니다:

```
Hello, World!
```

더 복잡한 디버그는 Logger 클래스를 사용하여 수행할 수 있습니다.

```Java
import java.util.logging.Logger;

public class DebugExample {
    private static final Logger LOG = Logger.getLogger(DebugExample.class.getName());

    public static void main(String[] args) {
        LOG.info("This is a debug message");
    }
}
```

이 경우, 아래와 같은 메시지가 출력됩니다:

```
INFO: This is a debug message
```

## 심층 탐구:

1) 디버그 출력은 프로그래밍의 초기 시기부터 사용되었습니다. 
2) 대안으로는 로그 파일에 메시지를 출력하거나 웹 서비스를 사용하여 디버그 정보를 실시간으로 전송하는 것 등이 있습니다. 
3) Java에서는 다양한 레벨의 로깅 메시지를 생성하는 로거 클래스와 다양한 출력 경로를 제공하는 핸들러 클래스를 제공하여 핵심 디버그 출력 기능을 구현합니다.

## 참고 자료:

1) [Oracle Java Docs](https://docs.oracle.com/javase/8/docs/api/java/util/logging/package-summary.html)
2) [Java Debugging Guide](https://www.baeldung.com/java-application-logging)
3) [Java Tutorials - Debugging](https://docs.oracle.com/javase/tutorial/getStarted/debug/index.html)