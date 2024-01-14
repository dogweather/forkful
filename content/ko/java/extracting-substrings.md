---
title:                "Java: 하위 문자열 추출하기"
simple_title:         "하위 문자열 추출하기"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/extracting-substrings.md"
---

{{< edit_this_page >}}

"## 왜 substring 추출이 필요한가요?"

substring 추출은 문자열에서 원하는 부분만을 따로 추출하는 것을 말합니다. 이는 문자열 데이터를 다루는 프로그래머에게는 중요한 기술이며, 특정 조건에 따라 원하는 정보만을 추출하는 작업에 많이 사용됩니다. 예를 들어, 사용자가 입력한 문자열에서 이름만을 추출하거나, 특정 단어를 포함하는 문자열만을 추출하는 등 다양한 상황에서 유용하게 사용됩니다.

"## 어떻게 substring을 추출할 수 있나요?"

자바에서 substring을 추출하기 위해서는 String 클래스의 substring() 메소드를 사용하면 됩니다. 이 메소드는 두 개의 매개변수를 받는데, 첫 번째 매개변수는 시작 인덱스, 두 번째 매개변수는 마지막 인덱스입니다. 예를 들어, 아래의 코드는 문자열에서 3번째부터 6번째 글자까지를 추출하는 예제입니다.

```Java
String str = "Hello World";
String extracted = str.substring(2, 6);
System.out.println(extracted);
```
출력 결과는 "llo W"가 될 것입니다.

이외에도 많은 예시를 만들어보면서 substring을 추출하는 방법을 익힐 수 있습니다. 또한, 이 메소드를 활용하여 문자열의 길이나 특정 문자열의 존재 여부 등에 대한 정보를 추출할 수도 있습니다.

"## substring 추출의 깊은 세부사항"

substring을 추출하는 과정에서 중요한 점은 시작 인덱스와 마지막 인덱스를 잘 파악하는 것입니다. 이는 추출하려는 문자열의 시작과 끝을 정확하게 지정해야 원하는 결과값을 얻을 수 있기 때문입니다. 또한, 인덱스에 대한 개념이 없는 경우 오류가 발생할 수 있으므로 항상 주의하는 것이 좋습니다.

그 외에도 substring 추출을 위해 사용할 수 있는 다양한 메소드나 라이브러리들이 있으니 자세한 내용은 검색하여 공부해보시기 바랍니다.

"## 더보기"

- [Java String 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [substring 추출 관련 예제 코드](https://www.programiz.com/java-programming/examples/substring)
- [언어별 substring 추출 방법 비교](https://social.msdn.microsoft.com/Forums/ko-KR/256b2ef0-173c-4ff9-a1d3-753cfd4140ee/javalangstringsubstring?forum=programmingkr)