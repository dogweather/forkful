---
title:                "표준 에러에 쓰는 것"
html_title:           "Java: 표준 에러에 쓰는 것"
simple_title:         "표준 에러에 쓰는 것"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?: 
표준 에러로 쓰는 것은 프로그래머가 프로그램 실행 중에 오류와 디버그 메시지를 출력할 수 있도록 하는 것입니다. 이는 프로그래머가 코드를 디버그하고 문제를 해결하는데 도움이 됩니다. 

## 하는 방법: 
```java
System.err.println("이것은 표준 에러로 출력됩니다.");
```
위의 코드는 "이것은 표준 에러로 출력됩니다."라는 메시지를 표준 에러로 출력하는 예제입니다.

**출력:**

```
이것은 표준 에러로 출력됩니다.
```

## 깊게 파고들기:
표준 에러를 사용하는 것은 프로그래밍에서 오랜 역사를 가지고 있습니다. 과거에는 프린터나 터미널과 같은 장치를 통해 오류 메시지를 출력했지만, 이제는 오류를 표준 에러로 출력하는 것이 일반적입니다. 또한 표준 출력과 표준 에러를 분리하여 보는 것이 좋습니다. 그렇지 않으면 프로그램 실행 결과와 오류 메시지가 서로 섞이게 될 수 있습니다.

대안으로는 표준 에러 대신 로깅 라이브러리를 사용하는 것이 있습니다. 로깅 라이브러리는 오류 메시지를 기록하고 저장하여 추후에 분석하는데 도움을 줍니다. 

표준 에러에 데이터를 출력하려면 System.err 객체를 사용하면 됩니다. 이 객체는 표준 에러 스트림에 접근할 수 있는 메서드를 제공합니다.

## 관련 자료: 
- [Java 표준 에러 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Java 로깅 라이브러리](https://logging.apache.org/log4j/2.x/)