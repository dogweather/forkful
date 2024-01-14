---
title:    "Ruby: 문자열을 소문자로 변환하기"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 작업은 프로그래밍에서 매우 유용합니다. 이를 통해 사용자로부터 입력 받은 문자열을 일관된 소문자 형태로 처리할 수 있으며, 데이터베이스에서 검색을 할 때도 대/소문자 구분 없이 원하는 결과를 얻을 수 있습니다.

## 어떻게

```Ruby
str = "Hello, World!"
puts str.downcase
```

출력: hello, world!

위 예제는 `String` 클래스의 `downcase` 메소드를 사용하여 문자열을 소문자로 변환하는 간단한 방법을 보여줍니다.

```Ruby
str = "안녕하세요"
puts str.downcase
```

출력: 안녕하세요

다른 언어와 달리 루비에서는 한글을 사용해도 `downcase` 메소드가 제대로 동작합니다. 이는 루비가 유니코드를 기본적으로 지원하기 때문인데, 유니코드를 다루는 데 있어서는 루비가 다른 언어보다 우수한 성능을 발휘합니다.

## 깊게 파고들기

문자열을 소문자로 변환하는 방법은 간단하지만, 내부적으로 어떻게 동작하는지 알아볼까요? 루비에서는 `String` 클래스의 `downcase` 메소드를 호출하면, 해당 문자열의 유니코드를 순회하며 대문자를 소문자로 변환하는 과정을 거칩니다. 이 과정에서는 원래 문자열은 그대로 두고, 복사된 새로운 문자열을 반환합니다.

## 참고하기

- [루비 안내서: 문자열 변환하기](https://ruby-doc.org/core-2.7.2/String.html#method-i-downcase)
- [루비와 유니코드](https://guides.rubyonrails.org/active_support_core_extensions.html#unicode)