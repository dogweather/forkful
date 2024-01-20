---
title:                "문자열 대문자화"
html_title:           "Ruby: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
문자열 대문자화는 그 문자열의 첫 글자를 대문자로 바꾸는 것을 말합니다. 프로그래머들은 이를 통해 텍스트를 이해하거나 분석하기 쉽게 만들 때 주로 사용합니다.

## 어떻게 하나요:
다음은 문자열 "hello"를 대문자로 바꾸는 방법입니다:

```Ruby
"hello".capitalize
```

이 코드의 실행 결과는 다음과 같습니다:

```Ruby
"Hello"
```

## 디피 다이브:
문자열 대문자화 기능은 Ruby의 '작은' 개선 사항 중 하나입니다. 초기 프로그래밍 언어에는 이런 기능이 없었고, 대소문자를 구분하려면 개발자가 직접 구현해야 했습니다.

`capitalize` 외에도 문자열의 모든 단어의 첫 글자를 대문자로 만들고 싶다면 `titleize`를 사용할 수 있습니다. 하지만 이 기능은 Rails에서만 제공되므로 순수 Ruby에서는 사용하실 수 없습니다. 

내부적으로 `capitalize`는 문자열의 첫 번째 문자를 대문자로 바꾸고, 나머지 문자는 모두 소문자로 바꿉니다. 이를 위해 Ruby는 내부적으로 ASCII 또는 Unicode 표를 사용하여 대소문자를 구별합니다. 

## 참조 자료: 
1. [Ruby 문서: capitalize](https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize)
2. [Ruby 문서: titleize (Rails)](https://api.rubyonrails.org/classes/String.html#method-i-titleize)
3. [ASCII 표](https://ascii.cl/)
4. [Unicode 표](http://unicode.org/charts/)