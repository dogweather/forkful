---
title:                "문자열 대문자화"
html_title:           "Java: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

문자열 대문자화는 모든 문자를 대문자로 바꾸는 과정입니다. 프로그래머들은 사용자 입력을 표준화하거나 사용자와 상호작용할 때 대소문자를 통일하는 데 사용합니다.

## 어떻게 할까요?

아래는 Java에서 문자열 대문자화를 구현하는 간단한 예입니다:

```Java
public class Main {
    public static void main(String[] args) {
        String myString = "Java로 코딩하기!";
        String result = myString.toUpperCase();
        System.out.println(result);
    }
}
```

이 코드를 실행하면 다음 출력이 나타납니다:

```Java
JAVA로 코딩하기!
```

## 디테일 다이빙

- **역사적 맥락**: 문자열 대문자화는 프로그래밍의 초기 단계부터 사용되어 왔습니다. 기능이 간단해 보일 수도 있지만, 언어와 도메인에 따라 다양하게 구현되어 왔습니다.

- **대안**: Apache Commons Lang 라이브러리의 `StringUtils.upperCase()` 메소드도 문자열 대문자화 옵션으로 사용할 수 있습니다. 이것은 여러 언어에 대한 지원을 포함하므로 Java의 내장 메소드보다 유연성을 제공합니다.

- **구현 세부 정보**: Java에서 `toUpperCase()` 메소드는 문자열의 모든 문자를 대문자로 변환합니다. 이 메소드는 원래 문자열을 변경하지 않고, 모든 문자가 대문자로 변환된 새 문자열을 반환합니다.

## 참고 자료

- [Java String toUpperCase() 메서드](https://www.javatpoint.com/java-string-touppercase)
- [Apache Commons Lang StringUtils 클래스](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
- [SO: 언어에 의존하지 않는 문자열 대문자화](https://stackoverflow.com/questions/15210979/how-do-i-make-case-insensitive-string-in-java)