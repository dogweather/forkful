---
title:                "텍스트 검색 및 대체"
html_title:           "Ruby: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 바꾸는 작업은 많은 프로그래밍 언어에서 필수적인 작업입니다. 루비는 간단하고 강력한 메소드를 제공하여 텍스트를 검색하고 바꿀 수 있기 때문에, 더 효율적인 코딩 경험을 제공합니다.

## 사용 방법

```ruby
# 단어 "Hello"를 "Hola"로 바꿔주는 간단한 예제 코드
text = "Hello, world!"
puts text.gsub("Hello", "Hola") # "Hola, world!"가 출력됩니다.

# 특정 문자열을 삭제하는 예제 코드
text = "This is a sample sentence."
puts text.gsub("sample ", "") # "This is a sentence."가 출력됩니다.
```

위의 예제 코드에서 보듯이, `gsub` 메소드를 사용하면 텍스트를 쉽게 찾고 바꿀 수 있습니다. `gsub` 메소드의 첫 번째 인자는 바꾸고자 하는 문자열이고, 두 번째 인자는 그 문자열을 어떤 값으로 바꿀지를 나타냅니다. 또한, `gsub` 메소드는 문자열 내에서 모든 일치하는 부분을 바꾸기 때문에 한 번에 여러 개의 문자열을 바꿀 수 있습니다.

## 깊이 있는 설명

루비에서 `gsub` 메소드는 정규표현식도 사용할 수 있습니다. 예를 들어, `gsub(/[aeiou]/, "*")`와 같이 사용하면 문자열 내의 모음을 모두 `*`로 바꿀 수 있습니다. 따라서 특정 패턴에 일치하는 문자열을 한 번에 모두 바꿀 수 있습니다.

또한, `gsub` 메소드는 대소문자를 구분하지 않는 `i` 옵션을 지원합니다. 따라서 `gsub(/hello/i, "hola")`와 같이 사용하면 `hello`라는 단어만 찾아서 `hola`로 바꿀 수 있습니다.

## 참고 자료

- [Ruby regular expression reference](https://docs.ruby-lang.org/en/master/Regexp.html)
- [Ruby string methods](https://ruby-doc.org/core-2.7.0/String.html)

## 더 보기