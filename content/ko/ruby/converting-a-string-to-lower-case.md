---
title:                "Ruby: 문자열을 소문자로 변환하기"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

오늘은 Ruby 프로그래밍에 중요한 개념 중 하나인 문자열을 소문자로 변환하는 방법에 대해 알아보겠습니다.

## 왜
문자열을 소문자로 변환하는 이유는 간단합니다. 데이터를 일관성 있게 처리하기 위해서입니다. 대/소문자를 구분하지 않아야 하는 경우도 있기 때문에 문자열을 모두 소문자로 변환하면 효율적으로 데이터를 다룰 수 있습니다.

## 어떻게
Ruby에서 문자열을 소문자로 변환하는 방법은 간단합니다. `downcase` 메서드를 사용하면 됩니다. 코드 블록에서 다음과 같이 작성해보세요.

```Ruby
# 입력값으로 대문자가 포함된 문자열 선언
string = "Hello World"

# downcase 메서드를 사용하여 소문자로 변환
puts string.downcase
```

이 코드를 실행하면 "hello world"가 출력됩니다.

## 깊게 살펴보기
Ruby에서 문자열은 `String` 클래스로 다루어집니다. Ruby의 클래스는 많은 메서드를 갖고 있고, 그 중에는 `downcase`도 있습니다. 이 메서드는 문자열의 모든 알파벳을 소문자로 변환하는 역할을 합니다. 또한, `downcase!`와 같이 느낌표를 붙이면 원본 문자열을 변환하는 메서드가 됩니다. 이외에도 `upcase`와 `capitalize` 메서드도 있으니 다양한 변환 방법을 알아보세요.

## 더 알아보기
이외에도 Ruby에서 문자열을 다루는 다양한 방법이 있습니다. 아래의 링크들을 참고하여 더 많은 정보를 얻어보세요.

[Official Ruby Documentation about Strings](https://ruby-doc.org/core-3.0.0/String.html)

[Ruby API: String](https://rubyapi.org/3.0/o/string.html)

[Korea Ruby User Group (KORUG)](https://www.korug.org/)

## 참고
이번 포스트에서는 Ruby에서 문자열을 소문자로 변환하는 방법을 알아보았습니다. 문자열 다루는 또 다른 방법인 정규표현식도 함께 배워보세요. 감사합니다.