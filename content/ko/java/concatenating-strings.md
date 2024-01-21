---
title:                "문자열 연결하기"
date:                  2024-01-20T17:34:56.750105-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 연결(concatenating strings)은 두 개 이상의 문자열을 하나로 묶는 것을 말해요. 프로그래머는 메시지를 구성하거나, 데이터를 형식화할 때 문자열을 연결합니다.

## How to: (어떻게:)
```java
public class StringConcatExample {
    public static void main(String[] args) {
        String hello = "안녕";
        String world = "세계";
        String exclamation = "!";
        
        // + 연산자를 사용해서 문자열을 연결합니다.
        String greeting = hello + " " + world + exclamation;
        System.out.println(greeting);  // 출력: 안녕 세계!
        
        // StringBuilder를 사용하여 문자열 연결하기
        StringBuilder sb = new StringBuilder();
        sb.append(hello).append(" ").append(world).append(exclamation);
        System.out.println(sb.toString());  // 출력: 안녕 세계!
    }
}
```

## Deep Dive (심층 탐구)
과거에는 '+' 연산자가 문자열 연결의 주된 방법이었습니다. 하지만 많은 문자열을 연결할 때 성능 문제가 발생할 수 있죠. 자바 5부터는 `StringBuilder`가 성능 개선을 위해 등장했습니다. `StringBuffer`도 있지만, 멀티쓰레드 환경에서만 필요합니다. 내부적으로 `String` 클래스는 문자 배열을 사용하여 데이터를 처리하며, 불변(immutable)이죠. 그래서 기존 문자열에 변화를 주면 새로운 `String` 객체가 만들어지게 됩니다.

## See Also (더보기)
- [String concatenation in Java](https://www.baeldung.com/java-strings-concatenation)
- [StringBuilder vs String](https://www.geeksforgeeks.org/string-vs-stringbuilder-vs-stringbuffer-in-java/)
- [Java Documentation for StringBuilder](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/StringBuilder.html) 

(참고자료 링크는 학습을 더 확장하고 싶을 때 유용합니다. 영문 자료지만, 쉽게 따라 할 수 있어요.)