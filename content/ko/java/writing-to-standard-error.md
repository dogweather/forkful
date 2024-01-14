---
title:    "Java: 표준 오류에 쓰는 것"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

왜: 왜 누군가가 표준 오류로 쓰기에 참여할까요? 보통, 표준 오류는 프로그래밍 오류나 디버깅을 위해 사용됩니다. 표준 오류를 올바르게 사용하는 것은 오류를 파악하고 수정하는 데 있어서 매우 유용합니다.

### 왜

Java 프로그래밍에서 표준 출력과 표준 오류는 중요하게 사용되는 개념입니다. 표준 출력은 보통 콘솔 창이나 파일 등에 출력되는 결과를 의미합니다. 하지만 표준 오류는 오류나 예외 메시지를 의미합니다. 따라서, 표준 오류를 사용하여 오류나 버그를 파악하고 디버깅할 수 있습니다.

### 어떻게 사용하나요

만약 우리가 예기치 못한 오류나 예외 상황을 처리하고 싶다면, 우리는 표준 오류를 사용해야 합니다. 이를 위해서는 `System` 클래스에 있는 `err` 필드를 사용하면 됩니다. 예를 들어, 아래와 같이 사용할 수 있습니다.

```Java
System.err.println("이것은 예외 메시지입니다.");
```

위 코드를 실행하면, 표준 오류로 메시지가 출력될 것입니다. 이를 통해 우리는 예외 상황이 발생했음을 알 수 있고, 이를 처리할 수 있습니다.

### 깊게 살펴보기

`print` 메소드와는 달리, `println` 메소드는 메시지 끝에 줄바꿈 문자를 붙여 출력합니다. 따라서 `System.err.println()`으로 오류 메시지를 출력하면, 콘솔 창에 나타날 때 훨씬 더 직관적인 결과를 볼 수 있습니다. 또한 `System.err`은 `PrintStream` 클래스의 인스턴스이므로, 다양한 출력 메소드를 활용할 수 있습니다.

### 참고 자료

- [Java 공식 문서 - System 클래스](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Java 공식 문서 - PrintStream 클래스](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [Java 표준 오류에 대한 더 자세한 설명](https://www.edureka.co/community/391/what-is-standard-error-in-java)
- [Java 프로그래밍의 표준 출력과 표준 오류](https://www.geeksforgeeks.org/standard-output-streams-in-java/)