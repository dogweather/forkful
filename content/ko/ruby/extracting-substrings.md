---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

# 루비에서 부분 문자열 추출하기: 기본 안내서 
## 다람쥐 추출이란 무엇이며 왜 필요한가? 

부분 문자열(substring) 추출은 주어진 문자열에서 특정 부분을 뽑아내는 방법입니다. 프로그래머들은 데이터 정리, 파싱 혹은 데이터 에러 수정 등 다양한 이유로 이를 사용합니다.

## 어떻게 하는가: 

루비에서 부분 문자열을 추출하는 가장 일반적인 방법은 `slice` 메서드입니다. 

```ruby
str = "Hello, Ruby programmer"
puts str.slice(7, 4) 
```

위 코드를 실행하면, "Ruby"라는 출력 결과를 볼 수 있습니다. 

## 깊게 들어가면: 

### * 역사적 배경 : 
기본 문자열 추출 방법은 루비 언어가 처음 나온 이래로 사용되고 있습니다. 초기 프로그래밍 언어부터 문자열 조작의 핵심 요소였습니다.

### * 대안 : 
`slice` 외에도 `[]` 연산자를 이용하여 부분 문자열을 추출할 수 있습니다.

```ruby
str = "Hello, Ruby programmer"
puts str[7, 4] 
```

### * 구현 세부사항 : 
부분 문자열을 추출하는 데는 인덱스 위치와 길이가 필요합니다. 인덱스 위치는 문자열에서 추출할 부분의 시작점이며, 길이는 얼마나 많은 문자를 가져올 것인지를 결정합니다.

## 추천 자료: 

1. [Ruby 문서 - String](https://ruby-doc.org/core/String.html): 이 사이트에서는 루비의 String 클래스에 대한 공식 문서를 보실 수 있습니다.
2. [루비 문자열 쪼개기](https://www.rubyguides.com/2018/01/ruby-string-methods/#ruby-split): this is a good detailed guide on working with Strings in Ruby.
3. [루비에서 `slice` 사용 예제](https://www.geeksforgeeks.org/ruby-string-slice-function-with-examples/): this site provides a detailed explanation and examples of `slice` usage in Ruby.