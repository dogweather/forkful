---
date: 2024-01-20 17:42:28.327212-07:00
description: "\uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790 \uC0AD\uC81C\uB294 \uC8FC\
  \uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uD328\uD134\uC774\uB098\
  \ \uBB38\uC790\uB97C \uCC3E\uC544 \uC81C\uAC70\uD558\uB294 \uACFC\uC815\uC785\uB2C8\
  \uB2E4. \uC774\uB294 \uB370\uC774\uD130 \uC815\uC81C, \uC785\uB825 \uC720\uD6A8\uC131\
  \ \uAC80\uC0AC, \uB610\uB294 \uD2B9\uC815 \uD615\uC2DD\uC5D0 \uB9DE\uCDB0 \uB370\
  \uC774\uD130\uB97C \uD45C\uD604\uD560 \uB54C \uC8FC\uB85C \uC0AC\uC6A9\uB429\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:28.943660-06:00'
model: gpt-4-1106-preview
summary: "\uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790 \uC0AD\uC81C\uB294 \uC8FC\uC5B4\
  \uC9C4 \uBB38\uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uD328\uD134\uC774\uB098 \uBB38\
  \uC790\uB97C \uCC3E\uC544 \uC81C\uAC70\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4\
  . \uC774\uB294 \uB370\uC774\uD130 \uC815\uC81C, \uC785\uB825 \uC720\uD6A8\uC131\
  \ \uAC80\uC0AC, \uB610\uB294 \uD2B9\uC815 \uD615\uC2DD\uC5D0 \uB9DE\uCDB0 \uB370\
  \uC774\uD130\uB97C \uD45C\uD604\uD560 \uB54C \uC8FC\uB85C \uC0AC\uC6A9\uB429\uB2C8\
  \uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
패턴에 맞는 문자 삭제는 주어진 문자열에서 특정 패턴이나 문자를 찾아 제거하는 과정입니다. 이는 데이터 정제, 입력 유효성 검사, 또는 특정 형식에 맞춰 데이터를 표현할 때 주로 사용됩니다.

## How to: (방법)
Java에서 문자열 패턴을 삭제하는 간단한 방법은 `replaceAll()` 메소드를 사용하는 것입니다. 정규 표현식을 이용해 매치되는 모든 문자를 삭제할 수 있어요.

```java
public class PatternDeletionExample {
    public static void main(String[] args) {
        String example = "Hello123 456World!";
        String pattern = "\\d+"; // 숫자에 매치되는 정규식 패턴
        String modified = example.replaceAll(pattern, "");

        System.out.println(modified); // "Hello World!"
    }
}
```

출력:
```
Hello World!
```

또 다른 예시:
```java
public class SpecialCharsRemoval {
    public static void main(String[] args) {
        String example = "K-pop is $100% awesome!";
        String pattern = "[^a-zA-Z\\s]"; // 영어와 공백을 제외한 모든 문자에 매치
        String modified = example.replaceAll(pattern, "");

        System.out.println(modified); // "Kpop is  awesome"
    }
}
```

출력:
```
Kpop is  awesome
```

## Deep Dive (심층 분석)
정규 표현식은 문장 안에서 특정 패턴을 찾는 강력한 방법입니다. 자바에서 `Pattern`과 `Matcher` 클래스가 있지만 문자열 클래스 내내 편리한 메소드들이 많아 대부분의 경우 이를 사용합니다.

역사적으로 볼 때, 정규 표현식은 1950년대 후반 제안되었고, 1970년대에 Unix 환경에서 널리 퍼졌습니다. 자바에서는 주로 `java.util.regex` 패키지 안에 있는 클래스를 통해 이를 구현합니다.

문자 삭제 외에도 문자 대체나 분할 등에도 사용할 수 있습니다. 예를 들어, `String[] words = sentence.split("\\s+");`는 문장을 공백을 기준으로 나누는 코드입니다.

`replaceAll()`은 내부적으로 `java.util.regex` 패키지의 기능을 사용합니다. 문자열이 길고, 수행해야 할 정규식 작업이 많은 경우, 직접 `Pattern` 및 `Matcher` 클래스를 사용하는 것이 성능상 이점을 가질 수 있습니다.

## See Also (참고 자료)
- [Java String `replaceAll()` Method](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
- [Java Pattern Class Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
