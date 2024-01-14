---
title:                "Ruby: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 일은 프로그래밍에서 매우 중요합니다. 이것은 데이터 요청 및 처리에서 사용되는 문자열의 크기를 알고 싶을 때 유용합니다. 또한 이것은 코드를 디버깅 할 때 많은 도움이 됩니다. 이 글에서는 Ruby를 사용해서 문자열의 길이를 찾는 방법에 대해 배워보겠습니다.

## 어떻게

먼저, `length` 메소드를 사용하여 문자열의 길이를 찾을 수 있습니다. 예제 코드를 보며 살펴보겠습니다.

```Ruby
string = "안녕하세요"
puts string.length
```
출력 결과: `5`

위의 코드에서 우리는 "안녕하세요"라는 문자열의 길이가 5라는 것을 알 수 있습니다. `length` 메소드는 문자열을 구성하는 문자의 개수를 세는 역할을 합니다. 따라서 글자 한글자가 하나의 문자로 인식되기 때문에 위 예제에서 우리는 5라는 결과를 얻게 됩니다.

이제 `size` 메소드도 사용해보겠습니다. `length`와 마찬가지로 문자열의 길이를 반환하는 역할을 합니다. 코드 예제를 보겠습니다.

```Ruby
string = "안녕하세요"
puts string.size
```
출력 결과: `5`

위의 코드를 보면 `length` 메소드와 동일한 결과를 얻게 됩니다. `length`와 `size`의 차이점은 입력 값이 없어도 모든 클래스에서 `length`를 사용할 수 있지만, `size`는 반드시 `Enumerable` 모듈이 사용 가능한 클래스에서만 사용할 수 있다는 것입니다.

## Deep Dive

위에서 언급한 예시 외에도 더 많은 방법으로 문자열의 길이를 찾을 수 있습니다. 예를 들어, `String#bytesize` 메소드를 사용하여 바이트 길이를 알 수 있습니다. 즉, 문자열을 바이트로 인코딩했을 때의 길이를 말합니다. 또 다른 예시로는 `StringIO` 클래스를 사용해서도 문자열의 길이를 알 수 있습니다.

추가적으로, 유니코드에서 문자열의 길이를 찾는 방법도 있습니다. 손쉽게 `chars` 메소드를 사용하여 불리언 값으로 음절의 문자열의 길이를 구할 수 있습니다. 이 외에도 다양한 방법이 존재하며, 자유롭게 탐색해보세요!

## See Also

- [Ruby 문서 - 문자열 관련 메소드](https://ruby-doc.org/core-2.7.2/String.html)
- [Ruby - 문자열 길이 찾기](https://www.rubyguides.com/2018/10/string-length-methods/)
- [Ruby 문서 - Enumerable 모듈](https://ruby-doc.org/core-2.7.2/Enumerable.html)