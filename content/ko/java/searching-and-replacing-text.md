---
title:                "Java: 텍스트 검색 및 대체 (Tekseuteu Geomsae Mit Daecha)"
simple_title:         "텍스트 검색 및 대체 (Tekseuteu Geomsae Mit Daecha)"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 바꾸는 작업에 참여하는 이유는 간단합니다. 이 작업을 통해 특정 텍스트를 빠르고 쉽게 찾아서 다른 텍스트로 대체할 수 있기 때문입니다.

## 하기

자바에서 텍스트를 검색하고 바꾸는 것은 간단한 작업입니다. 먼저, 필요한 클래스를 import 해야합니다.

```Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;
```

다음으로, 대체하고 싶은 텍스트를 찾을 정규표현식을 작성해야 합니다. 예를 들어, "Hello"를 "안녕하세요"로 바꾸고 싶다면 다음과 같이 작성할 수 있습니다.

```Java
Pattern pattern = Pattern.compile("Hello"); // Hello를 찾는 패턴
```

그리고 대체할 텍스트를 바꾼 후 결과를 저장할 변수를 만들어야 합니다. 아래의 코드에서는 "안녕하세요"를 찾으면 그 부분을 "Hello"로 바꿔서 replace라는 변수에 저장합니다.

```Java
String replace = matcher.replaceAll("Hello"); // "안녕하세요"를 "Hello"로 바꿈
```

마지막으로, 이 과정을 적용할 텍스트를 입력하고, 결과를 출력합니다.

```Java
String input = "안녕하세요, Hello"; // 텍스트 입력
System.out.println(replace); // 결과 출력
```

위의 코드를 실행하면 "Hello, Hello"가 출력됩니다.

## 심층 탐구

이제 우리는 간단하게 텍스트를 찾고 바꾸는 방법을 배웠습니다. 그러나 실제로는 이 과정이 더 복잡할 수 있습니다. 예를 들어, 대소문자를 구분하거나 특수 문자를 고려해야 할 수도 있습니다. 이럴 때는 정규표현식을 더욱 자세하게 작성해야 합니다. 또한, 여러 개의 문자열을 한 번에 대체하고 싶다면 패턴과 대체 문자열을 배열로 입력하여 한 번에 바꿀 수도 있습니다.

## 봐주세요

- [Oracle Java 레퍼런스](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [정규표현식 연습 사이트](https://regex101.com/)
- [정규표현식 패턴 생성 사이트](https://regexr.com/)