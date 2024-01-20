---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요? (What & Why?)

문자열의 길이를 찾는 것은, 어떤 문자열이 포함된 문자의 개수를 알아내는 것을 의미합니다. 이는 항목을 순환하거나, 배열 또는 문자열을 조작할 때 굉장히 중요한 역할을 합니다.

## 어떻게 하는 건가요? (How to)

Java에서 문자열의 길이를 찾는 방법은 `length()`라는 메소드를 사용하는 것입니다. 예를 들어,

```Java
public class Main {
    public static void main(String[] args) {
        String str = "프로그래밍";
        int length = str.length();
        System.out.println("문자열의 길이: " + length);
    }
}
```

위 코드를 실행하면 다음과 같이 출력됩니다: 

```Java
문자열의 길이: 5
```

##깊이 들여다보기 (Deep Dive)

1. 역사적 맥락: `length()` 메소드는 Java가 처음 만들어질 때부터 존재했으며, 문자열의 길이를 찾는 기본적이면서도 효과적인 방법입니다.

2. 대체 방법: 문자열의 길이를 찾는 차선책으로 `toCharArray()` 메소드를 사용하는 방법이 있습니다. 이는 문자열을 문자 배열로 변환하여 그 길이를 측정하는 방식입니다.

3. 동작 방식: `length()` 메소드는 내부적으로 문자열의 끝을 가리키는 위치에서 문자열의 시작을 가리키는 위치를 뺀 값을 반환합니다. 따라서, 이 연산의 시간 복잡도는 O(1)이고 실행 시간은 상수입니다.

## 참고하기 (See Also)

문자열과 관련된 Java의 기타 메소드를 더 배우려면 아래 링크를 참조하세요.
- [Oracle Java Documentation: String](https://docs.oracle.com/javase/9/docs/api/java/lang/String.html)
- [StackOverflow: length() in java](https://stackoverflow.com/questions/3793650/length-and-length-in-java)