---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:45:46.795797-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열에서 특정 부분을 추출하는 것을 우리는 서브스트링(substring)이라고 합니다. 프로그래머들은 데이터를 분석하거나 특정 패턴을 찾아낼 때 이 기능을 자주 사용합니다.

## How to: (방법)
```java
public class SubstringExample {
    public static void main(String[] args) {
        String message = "안녕하세요, 자바 프로그래밍!";
        String greeting = message.substring(0, 5); // "안녕하세요"
        String hobby = message.substring(10);      // "자바 프로그래밍!"
        
        System.out.println(greeting);  // 출력: 안녕하세요
        System.out.println(hobby);     // 출력: 자바 프로그래밍!
    }
}
```

## Deep Dive (심층 분석)
자바에서 substring을 추출하는 기능은 초기 버전부터 있었습니다. `String.substring(int beginIndex, int endIndex)`와 `String.substring(int beginIndex)` 오버로드 메소드를 이용해 사용합니다. 두 메서드 모두 내부적으로 원본 문자열의 일부를 가리키는 새로운 문자열을 반환합니다. 이전에는 substring 작업이 문자열의 복사본을 생성했지만, 자바 7부터는 문자열을 공유하도록 최적화되었습니다. 그런데, 메모리 누수 문제 때문에 자바 7 이후에 이러한 공유 메커니즘이 제거되고, 항상 새로운 문자열을 생성하도록 변경되었습니다.

대안으로는 Apache Commons Lang과 같은 서드파티 라이브러리를 사용할 수 있으며, 다양한 문자열 처리 유틸리티를 제공합니다.

## See Also (참조)
- [Oracle Java Documentation – String](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Apache Commons Lang – StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
