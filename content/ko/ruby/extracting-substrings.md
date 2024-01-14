---
title:    "Ruby: 부분 문자열 추출"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열 추출을 하려는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 특정 단어를 찾기 위해 문자열에서 부분 문자열을 추출하거나, 문자열을 다른 포맷으로 변환하기 위해 추출하는 경우가 있을 수 있습니다.

## 추출하는 방법

Ruby에서는 `slice` 메소드를 사용하여 문자열에서 부분 문자열을 추출할 수 있습니다. 아래는 `slice` 메소드를 사용하여 문자열을 추출하는 간단한 예제 코드입니다.

```Ruby
str = "안녕하세요! 반갑습니다."
puts str.slice(0, 8) # "안녕하세요"
```

위의 코드에서 `slice` 메소드는 첫 번째 매개변수에 시작 인덱스를, 두 번째 매개변수에 추출할 문자열의 길이를 받습니다. 시작 인덱스는 0부터 시작하며, 추출할 문자열의 길이는 생략할 경우 해당 인덱스부터 문자열의 끝까지 추출합니다.

추출된 부분 문자열을 사용하지 않고, 원본 문자열을 그대로 사용하고 싶을 경우 `slice!` 메소드를 사용할 수도 있습니다. 이 경우, 원본 문자열에서 추출된 부분 문자열이 삭제되므로 주의해야 합니다.

```Ruby
str = "안녕하세요! 반갑습니다."
str.slice!(0, 8) # "안녕하세요"
puts str # " 반갑습니다."
```

## 깊이 들어가기

위에서 소개한 `slice` 메소드의 기능을 확장할 수 있는 다른 유용한 메소드들도 있습니다.

### `split`

`split` 메소드를 사용하면 문자열을 구분자를 기준으로 쪼개어 배열로 반환할 수 있습니다. 아래는 공백을 기준으로 문자열을 쪼개는 예제 코드입니다.

```Ruby
str = "Hello World!"
arr = str.split(" ") # ["Hello", "World!"]
```

문자열을 쪼개지 않고 그대로 사용하고 싶을 때는 구분자를 생략할 수 있습니다.

### `gsub`

`gsub` 메소드는 정규표현식을 사용하여 문자열을 대체하는 메소드입니다. 아래는 문자열에 있는 모든 공백을 `_`로 대체하는 예제 코드입니다.

```Ruby
str = "Hello World!"
str.gsub(/ /, "_") # "Hello_World!"
```

자세한 정규표현식의 사용법은 다른 글에서 다루겠지만, 이번 포스트에서는 링크를 통해 알아보는 것으로 마무리하겠습니다.

## 참고자료

[Ruby String Class Documentation](https://ruby-doc.org/core-2.7.2/String.html)

[Ruby Regular Expression Tutorial](https://ruby-doc.com/docs/ProgrammingRuby/html/intro.html)