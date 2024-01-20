---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

패턴에 일치하는 문자를 삭제하는 것은 특정 기준에 따라 텍스트에서 문자를 제거하는 프로그래밍 기법입니다. 프로그래머들은 불필요한 공백, 특수 문자를 제거하거나 데이터 정제를 위해 이를 사용합니다.

## 어떻게 :

아래에 Java에서 패턴에 일치하는 문자를 삭제하는 간단한 코드 조각이 있습니다.

```Java
public class Main {
    public static void main(String[] args) {
        String str = "안녕하세요, 웹사이트에 오신 것을 환영합니다!";
        String pattern = "[^a-zA-Z0-9가-힣 ]";
        String cleanedStr = str.replaceAll(pattern, "");
        System.out.println(cleanedStr);
    }
}
```

이 스크립트를 실행하면 출력은 다음과 같습니다:
```
안녕하세요 웹사이트에 오신 것을 환영합니다
```

이는 공백과 기호를 제거하여 결과 문자열을 반환합니다.

## 깊은 탐구 :

1. 히스토리 : String 클래스의 replaceAll 메서드는 자바 1.4 버전부터 도입되었습니다. 이는 정규 표현식을 사용하여 문자열의 특정 패턴을 찾아 대체하는 기능을 제공합니다.
2. 대안 : Apache Commons Lang 라이브러리는 StringUtils 클래스를 제공하여 이와 같은 문제를 해결합니다. 하지만 기본 Java 라이브러리만을 이용하고 싶다면, replaceAll 메서드가 가장 간단한 해결 방법입니다.
3. 세부 구현 : replaceAll 메서드는 내부적으로 Pattern 클래스와 Matcher 클래스를 사용합니다. 이들은 각각 정규 표현식 패턴을 컴파일하고 패턴 검색을 수행하는 역할을 합니다.

## 참고자료 :

1. 자바 String 클래스 Dokumentation : [링크](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
2. 정규 표현식 튜토리얼 : [링크](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
3. Apache Commons Lang 라이브러리 : [링크](https://commons.apache.org/proper/commons-lang/)