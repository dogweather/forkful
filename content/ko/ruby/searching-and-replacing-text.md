---
title:                "텍스트 검색 및 교체"
html_title:           "Ruby: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
텍스트 검색 및 대체란 무엇인가요?
텍스트 검색 및 대체는 프로그래머가 특정 문자나 단어를 다른 문자나 단어로 바꾸는 작업을 말합니다. 
이를 통해 코드를 더 효율적이고 간결하게 만들 수 있습니다. 

## 어떻게:
```Ruby
# 대문자를 소문자로 바꾸는 예제
text = "HELLO WORLD"
puts text.downcase
# output: hello world

# 문자를 다른 문자로 바꾸는 예제
text = "I love Ruby"
puts text.gsub("love", "like")
# output: I like Ruby
```

## 깊게 들어가기:
1. 역사적 배경: 텍스트 검색 및 대체는 단순한 문자열 처리 방법이 아니라 프로그래밍 언어의 발전과 함께 발전해온 기능입니다.
2. 대체 방법의 대안: 텍스트 검색 및 대체 외에도 정규 표현식, split/join 등 다양한 방법으로 문자열을 처리할 수 있습니다.
3. 구현 세부사항: Ruby에서는 gsub 메소드를 사용하여 문자열을 대체할 수 있으며, 정규 표현식을 활용할 수도 있습니다.

## 관련 자료:
- [Ruby Documentation](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Regular Expressions in Ruby](https://medium.com/@rameshdharan/regularexpressions-in-ruby-df79635c7dcc)
- [Splitting and Joining Strings in Ruby](https://ruby-doc.org/core-2.7.0/String.html#method-i-split)