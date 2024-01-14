---
title:    "Gleam: 문자열을 소문자로 변환하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 왜: 문장을 소문자로 변환하는 이유

일반적으로 프로그래밍에서는 대문자와 소문자를 구분하여 사용되는 경우가 많습니다. 이는 데이터를 정확하게 처리하기 위해서입니다. 때로는 대문자로 된 문자열을 가져와서 소문자로 변환해야 하는 경우가 있습니다. 이를 보다 쉽게 처리하기 위해 Gleam에서 제공하는 기능이 있습니다.
 
## 어떻게 하나요?

문자열을 소문자로 변환하는 것은 매우 간단합니다. 아래의 코드를 참고해보세요.

```Gleam
string = "GLEAM PROGRAMMING"
lowercase = string |> String.to_lower
```

위의 코드를 실행하면 "GLEAM PROGRAMMING"이 "gleam programming"으로 변환되는 것을 확인할 수 있습니다.

## 깊게 파헤쳐보기

Gleam에서는 문자열 변환을 위한 다양한 함수를 제공합니다. 이 중 가장 많이 사용되는 함수는 ```String.to_lower``` 함수입니다. 하지만 이 함수는 모든 문자를 소문자로 변환하는 것이 아니라 알파벳만 소문자로 변환합니다. 예를 들어, ```String.to_lower("HELLO 123")```를 실행하면 "hello 123"이 아닌 "hello 123"이 반환됩니다. 따라서 모든 문자를 소문자로 변환하려면 ```String.to_lower_ascii``` 함수를 사용해야 합니다.

그 밖에도 문자열을 대문자로 변환하는 ```String.to_upper``` 함수와 알파벳의 첫 글자만 대문자로 변환하는 ```String.to_capitalized``` 함수도 제공됩니다. 이와 같은 함수들을 적절히 사용하여 문자열을 원하는 형태로 변환할 수 있습니다.

# 더 알아보기

위에서 소개한 함수 이외에도 Gleam에서는 문자열을 처리하기 위한 다양한 함수를 제공합니다. 이를 더 자세히 알고 싶다면 아래의 링크들을 참고해보세요.

- [Gleam 공식 문서 - Strings](https://gleam.run/documentation/standard-library#strings)
- [Gleam 문자열 관련 함수의 예제](https://github.com/gleam-lang/gleam_stdlib/blob/main/test/strings_test.gleam)
- [Gleam 문자열 관련 함수의 소스 코드](https://github.com/gleam-lang/gleam_stdlib/blob/main/src/gleam_string.erl)

# 관련 링크

- [Gleam 언어 소개를 한국어로 읽어볼 수 있는 포스트](https://gleam.run/posts/2020-08-10-getting-started-with-gleam.html)
- [Gleam 언어의 기본 문법을 소개하는 동영상 자료](https://www.youtube.com/watch?v=DXLwOxkt8zE)