---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

category:             "Java"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열의 첫 글자를 대문자로 만드는 것을 말합니다. 보통 제목이나 문장을 쓸 때 규칙에 맞게 표현하기 위해 사용합니다.

## How to: (어떻게 하나요?)
자바에서 문자열의 첫 글자를 대문자로 만들어보겠습니다. 간단한 함수를 만들어 사용해 볼까요?

```java
public class CapitalizeExample {

    public static void main(String[] args) {
        String input = "hello, world!";
        String output = capitalize(input);
        System.out.println(output); // 출력: Hello, world!
    }

    public static String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}
```

## Deep Dive (심도있는 정보)
고대 로마에서 대문자가 먼저 쓰였고, 소문자는 나중에 발달했습니다. 자바에서는 `String` 클래스를 통해 쉽게 문자열을 처리할 수 있지만, `.toUpperCase()`, `.toLowerCase()` 같은 메소드는 지역 설정에 따른 언어 규칙을 따른다는 것을 명심해야 합니다. 대안으로 `StringUtils.capitalize()` 같은 Apache Commons Lang 라이브러리도 있습니다. 내부 구현상, `Character.toTitleCase()` 메소드를 사용하여도 비슷하게 첫 글자를 대문자로 만들 수 있습니다.

## See Also (관련 자료)
- [String Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/)
- [Character Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Character.html)
