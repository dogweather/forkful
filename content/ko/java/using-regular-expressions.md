---
title:                "정규 표현식 활용하기"
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 사용하는가?)
정규 표현식은 문자열에서 패턴을 찾기 위한 강력한 도구입니다. 프로그래머들은 데이터 유효성 검사, 문자열 검색 및 변환 작업을 효율적으로 수행하기 위해 이를 활용합니다.

## How to: (어떻게 사용하는가?)
```java
import java.util.regex.*;

public class RegexExample {
    public static void main(String[] args) {
        // 예제 1: 이메일 패턴 매칭
        Pattern pattern = Pattern.compile("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,6}$");
        Matcher matcher = pattern.matcher("example@email.com");
        System.out.println("이메일 유효성 검사: " + matcher.matches()); // true

        // 예제 2: 문자열에서 숫자 추출
        Pattern numPattern = Pattern.compile("\\d+");
        Matcher numMatcher = numPattern.matcher("주문 번호: 12345");
        if (numMatcher.find()) {
            System.out.println("추출된 숫자: " + numMatcher.group()); // 12345
        }
    }
}
```

## Deep Dive (심층 분석)
정규 표현식은 1950년대 후반에 만들어져 컴퓨터 과학에서 문자열 처리에 혁명을 일으켰습니다. 현재 `java.util.regex` 패키지를 주로 사용하지만, Apache Commons 라이브러리 같은 대안이 있습니다. Java에서 정규 표현식은 `Pattern`과 `Matcher` 클래스로 구현됩니다. `Pattern`은 표현식을 정의하고, `Matcher`는 문자열에 해당 패턴을 어떻게 적용할지를 결정합니다.

## See Also (추가 정보)
- [Java Pattern Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Java Matcher Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [정규 표현식 - 위키백과](https://ko.wikipedia.org/wiki/정규_표현식)
- [정규 표현식 101 (온라인 테스터)](https://regex101.com/)