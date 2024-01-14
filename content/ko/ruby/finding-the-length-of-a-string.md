---
title:                "Ruby: 문자열의 길이 찾기"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
왜 문자열의 길이를 찾는 일에 관여해야 하는지에 대해 1-2 문장으로 설명합니다. 

문자열의 길이를 찾기 위해서는 이해하기 쉬운 방법이며, 유용한 도구이기 때문에 프로그래머에게 매우 중요합니다.

## 방법
"```Ruby 
str = "안녕하세요"
puts str.length
```"

위의 예시 코드를 실행하면 "5"라는 결과가 출력됩니다. 여기서 "length"는 문자열의 길이를 나타내는 메소드입니다. 문자열의 각 문자를 하나씩 셀 필요가 없이 간단한 메소드를 사용하면 길이를 쉽게 찾을 수 있습니다. 

## 깊게 살펴보기
문자열의 길이를 찾는 방법에 대해 더 깊이 알아보겠습니다. "length" 메소드는 문자열 내의 모든 문자를 하나씩 셀 때 사용되는 방법 중 하나입니다. 또 다른 방법으로는 "size" 메소드를 사용할 수 있습니다. 두 메소드는 기능이 거의 동일하지만 "size" 메소드는 배열에서도 사용할 수 있습니다. 

## 관련 링크 
- [Ruby String 클래스 문서](https://ruby-doc.org/core-2.7.0/String.html)
- [공식 Ruby 문서 for .length](https://ruby-doc.org/core-2.7.0/String.html#method-i-length)
- [공식 Ruby 문서 for .size](https://ruby-doc.org/core-2.7.0/String.html#method-i-size)

## 참고 
그리스어 "metron"에서 유래된 "measuring rod"라는 뜻을 가지고 있는 단어 "메터"는 문자열을 셀 때 사용하는 다른 용어입니다. 이를 참고해보세요!