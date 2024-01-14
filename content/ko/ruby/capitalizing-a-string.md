---
title:    "Ruby: 문자열 대문자로 변환하기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

캐피탈라이징(captializing)은 루비(Ruby) 프로그래밍에서 매우 유용한 기능입니다. 문자열을 변경하거나 서식을 지정할 때 사용할 수 있어서, 코드의 유연성을 높여줍니다.

## 하우 투

먼저, `capitalize` 메소드를 사용하여 문자열을 대문자로 바꿀 수 있습니다. 아래의 코드를 참조하세요.

```Ruby
"hello world".capitalize # output: "Hello world"
```

이렇게 하면 첫 번째 단어만 대문자로 바뀝니다. 모든 단어를 대문자로 바꾸는 방법은 `upcase` 메소드를 사용하는 것입니다. 아래 코드를 확인하세요.

```Ruby
"hello world".upcase # output: "HELLO WORLD"
```

만약 첫 글자를 제외하고 나머지 문자를 소문자로 바꾸고 싶다면 `downcase` 메소드를 사용하세요. 아래 코드를 참고하세요.

```Ruby
"HELLO WORLD".downcase # output: "hello world"
```

한 글자만 대문자로 바꾸고 싶을 때는 `capitalize!` 메소드를 사용하세요. 아래는 예제 코드입니다.

```Ruby
"hello world".capitalize! # output: "Hello world"
```

마지막으로, 모든 단어에서 첫 글자만 대문자로 바꾸는 경우 `capitalize` 메소드에 `each_word` 메소드를 추가하면 됩니다. 아래 코드를 참조하세요.

```Ruby
"hello world".capitalize.each_word {|word| puts word} # output: "Hello World"
```

## 딥 다이브

위의 예제로는 간단한 문자열만 다루었지만, `capitalize` 메소드는 문자열의 길이를 확장하여 사용할 수 있습니다. 다양한 조건에 따라 문자열을 변경할 수 있으며, 여러 기능과 함께 사용할 수 있습니다. 더 자세한 정보는 공식 문서를 참조해주세요.

## 더 알아보기

만약 루비의 문자열 관련 메소드에 대해 더 알아보고 싶다면 아래 링크들을 참고해주세요.

- https://ruby-doc.org/core-2.7.1/String.html
- https://ko.wikipedia.org/wiki/루비_(프로그래밍_언어)
- https://www.fun-coding.org/PL&OOP5-Ruby.html

## 참고하기

이 글은 `ruby` 패키지의 `markdown` 형식으로 작성되었습니다. 자세한 사항은 `github`의 `markdown` 문서를 참고해주세요.