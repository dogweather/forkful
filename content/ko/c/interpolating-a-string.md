---
title:                "문자열 보간하기"
html_title:           "C: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 보간(interpolating a string)은 문자열의 일부분을 다른 값으로 대체하는 것을 말합니다. 예를 들어, "안녕하세요, [이름]"에서 [이름]의 자리에 실제 이름이 들어가게 하는 것입니다. 프로그래머들은 문자열 보간을 사용하여 텍스트를 동적으로 생성하거나 변수를 포함하는 메시지를 만들 때 유용하게 사용합니다.

## 방법:

```C
int age = 25;
printf("나는 %d살이다.", age);
```

위의 예제에서 "나는 [나이]살이다."라는 문자열에서 [나이]의 자리에 변수 age의 값이 들어가게 됩니다. 따라서 이 코드를 실행하면 "나는 25살이다."라는 메시지가 출력됩니다.

## 깊은 곳:

문자열 보간은 프로그래밍 언어의 일종인 코볼(COBOL)에서 처음 사용되었습니다. 현재는 대부분의 프로그래밍 언어에서 지원하고 있으며, 다른 방법으로는 문자열 연결(string concatenation)이 있습니다. 문자열 보간은 코드를 더 간결하고 가독성이 더 좋게 만들어주므로 프로그래머들 사이에서 인기가 많습니다. 구현 방식은 각 언어마다 다소 다를 수 있지만, 대부분 변수나 함수를 사용하여 구현됩니다.

## 더 보기:

- [문자열 보간 - 위키백과](https://ko.wikipedia.org/wiki/%EB%AC%B8%EC%9E%90%EC%97%B4_%EB%B3%B4%EA%B0%84)
- [코볼 - 위키백과](https://ko.wikipedia.org/wiki/%EC%BD%94%EB%B3%BC)