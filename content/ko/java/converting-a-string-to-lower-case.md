---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 소문자로 변환한다는 것은, 문자열의 모든 문자들을 소문자로 만드는 것입니다. 프로그래머들이 이 작업을 수행하는 주된 이유는, 데이터 정규화와 비교 작업에서 대소문자의 차이를 무시하기 위해서입니다.

## 어떻게:

```Java
public class Main {
    public static void main(String[] args) {
        String str = "Hello World!";
        String lowerCaseStr = str.toLowerCase();

        System.out.println(lowerCaseStr);
    }
}
```

이 코드를 실행하면 아래의 출력이 나옵니다:

```Java
hello world!
```

## 깊이 들여다보기

재미있는 사실은, 대소문자 변환의 개념이 제일 처음 소개된 건 1970년대, ASCII 코드가 개발된 이후였습니다. 알TERNATES에는 `Character.toLowerCase()` 또는 외부 라이브러리를 사용하는 방법 등이 있습니다. 자바의 toLowerCase 메소드는 Unicode 표준을 기반으로 문자를 소문자로 변환합니다. 그러나 주의해야 할 점은, 이 작업이 언어와 지역에 따라 다르게 작동할 수 있다는 것입니다.

## 참고 자료들

* Oracle Java Docs: [String](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#toLowerCase())
* StackOverflow: [When to use String.toLowerCase()](https://stackoverflow.com/questions/2978560/when-to-use-string-tolowercase)
* Oracle Java Docs: [Character](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/Character.html#toLowerCase(int))