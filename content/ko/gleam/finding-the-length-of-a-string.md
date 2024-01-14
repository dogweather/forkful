---
title:                "Gleam: 문자열의 길이 찾기"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열의 길이를 찾는 데 참여하려는 이유는 무엇인가요?

## 어떻게
```Gleam
let string = "안녕하세요";
let length = Gleam.Strings.length(string);

IO.inspect(length); // 결과: 5
```

자바스크립트와 달리, Gleam에서는 문자열의 길이를 바로 구할 수 있습니다. 문자열의 길이를 알고 있다면, 그 문자열을 얼마나 잘 처리할 수 있게 될까요?

## 깊게 들어가기
Gleam에서 문자열의 길이를 구하는 방법은 간단합니다. 하지만 실제로는 내부 로직이 어떻게 구현되고 있는지 궁금하실 것입니다. 알고리즘의 성능이나 다른 언어와의 비교는 무엇일까요? 문자열의 길이를 구하는 함수를 직접 만들어 보는 것도 재미있을 것입니다.

## 관련 자료
[Gleam Strings 모듈 참조](https://gleam.run/documentation/standard-libraries/strings/)