---
title:    "Swift: 정규 표현식 사용하기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 무엇일까요? 정규 표현식은 패턴을 나타내고 매칭하거나 바꾸는 작업에 매우 유용합니다. 예를 들어, 특정 문자열에서 이메일 주소를 찾아내거나, 전화번호를 국제 표준 형식으로 바꾸는 등의 작업에 매우 유용합니다.

## 이렇게 사용하세요

```Swift
let emailRegex = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}"

let string = "저의 이메일 주소는 abc123@gmail.com입니다."

let matches = string.matches(for: emailRegex)

// 결과
matches = ["abc123@gmail.com"]
```

위의 예시 코드에서 `matches(for:)` 함수는 해당 문자열에서 정규 표현식과 매칭된 부분을 찾아 배열로 반환합니다. 이를 통해 원하는 작업을 쉽게 수행할 수 있게 됩니다.

## 더 깊게 들어가보세요

정규 표현식은 매우 강력한 도구이지만, 익숙하지 않을 경우 약간의 학습이 필요할 수 있습니다. 특히, 일부 문법이 다른 프로그래밍 언어와 다를 수 있습니다. 하지만 그만큼 익숙해진 후에는 매우 유용한 도구가 될 것입니다.

## 더 보기

- "[정규표현식 - 위키백과](https://ko.wikipedia.org/wiki/%EC%A0%95%EA%B7%9C_%ED%91%9C%ED%98%84%EC%8B%9D)"
- "[Swift에서 정규표현식 사용하기](https://zeddios.tistory.com/7)"