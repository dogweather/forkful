---
title:    "Ruby: 문자열 대문자로 변환하기"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 것에 대해 알고 싶은 이유는 무엇일까요? 이 글에서는 그 이유와 그 방법에 대해 알아보겠습니다.

## 어떻게

문자열을 대문자로 바꾸는 것은 간단한 작업입니다. Ruby에서는 `.upcase` 메소드를 사용하여 간단하게 해결할 수 있습니다.

```Ruby
# 문자열 변수 정의
str = "hello world"

# 문자열을 대문자로 바꾸기
str.upcase

# 출력
HELLO WORLD
```

## 깊이 파고들기

문자열을 대문자로 바꾸는 과정에서 Ruby가 어떻게 코드를 실행하는지 알아보겠습니다. 우리는 `str.upcase` 메소드를 호출함으로써 문자열을 대문자로 바꿀 수 있지만, 실제로는 Ruby 내부에서 다음과 같은 과정이 일어납니다.

1. `String` 클래스 내부에서 `upcase` 메소드가 정의되어 있음을 확인합니다.
2. `str` 변수가 인자로 들어가며, `upcase` 메소드가 실행됩니다.
3. 메소드 내부에서 문자열을 대문자로 바꾸는 알고리즘을 수행합니다.
4. 대문자로 바뀐 문자열을 반환합니다.

이처럼 Ruby는 객체 지향 프로그래밍 언어이기 때문에 클래스와 메소드를 활용하여 간편하게 코드를 작성할 수 있습니다.

## 관련 글

- [Ruby 공식 문서 - String Class](https://ruby-doc.org/core-2.6/String.html)
- [Ruby 공식 문서 - String#upcase](https://ruby-doc.org/core-2.6/String.html#method-i-upcase)
- [Ruby Guid - Strings](https://www.rubyguides.com/ruby-tutorial/strings/)

## 관련 링크

- [Markdown 사용법](https://guides.github.com/features/mastering-markdown/)
- [Visual Studio Code 설치 및 설정 방법](https://www.44bits.io/ko/post/visual-studio-code-tutorial-for-beginners)