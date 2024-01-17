---
title:                "서브스트링 추출하기"
html_title:           "Java: 서브스트링 추출하기"
simple_title:         "서브스트링 추출하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

자바 프로그래밍에서 substrings을 추출하는 것은 문자열에서 특정 부분만을 분리하는 것을 의미합니다. 이를 통해 우리는 문자열을 더 쉽게 다룰 수 있습니다.

이 작업을 하는 이유는 여러 가지가 있지만 가장 큰 이유는 우리가 목표로 하는 문자열의 일부분만을 다루고자 할 때입니다. 예를 들어, 여러분이 어떤 문장에서 특정 단어만을 가져오고 싶을 때, substrings을 추출하는 것은 매우 유용합니다.

# 하는 법:

```
// 문자열에서 부분 추출 예제

public class SubstringExample {
    public static void main(String[] args) {

        String sentence = "Java Programming is fun";
        String substring = sentence.substring(5, 16);
        
        System.out.println(substring);
        
        // 출력 결과: Programming
    }
}
```

위 코드에서는 문자열에서 5번째부터 16번째까지의 문자만을 추출하여 substring 변수에 저장하고 있습니다. 이렇게 함으로써 우리는 sentence 변수에 저장된 문자열을 더 쉽게 다룰 수 있습니다.

# 깊이 들어가보기:

1) 역사적 배경: substrings을 추출하는 개념은 프로그래밍 언어마다 조금씩 다를 수 있지만, 대부분의 언어에서 지원하고 있습니다. 예를 들어, C 언어에서는 문자열을 포인터로 다루기 때문에 문자열에서 원하는 부분만을 추출하기가 어려웠지만, 자바에서는 문자열을 메모리에 저장하는 클래스가 있기 때문에 더 쉽게 추출할 수 있습니다.

2) 대안: substrings을 추출하는 것은 문자열을 다루기 위한 가장 기본적인 방법 중 하나입니다. 다른 대안으로는 정규식을 사용하는 방법이 있습니다. 정규식을 사용하면 문자열에서 특정 패턴을 검색하고 추출할 수 있습니다.

3) 구현 세부사항: 자바에서는 String 클래스에 내장된 substring() 메소드를 사용하여 서브스트링을 추출할 수 있습니다. 이 메소드는 첫 번째 매개변수로 시작 인덱스를, 두 번째 매개변수로 끝 인덱스를 전달받습니다. 또한, substring() 메소드는 문자열의 시작 인덱스를 0부터 셀 뿐만 아니라 마지막 인덱스를 포함하지 않는다는 것에 주의해야 합니다.

# 관련 자료:

- [Java String 클래스](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java 정규식](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)