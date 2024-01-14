---
title:    "Ruby: 텍스트 검색 및 교체"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜?

텍스트를 검색하고 대체하는 작업의 이유는 간단합니다 - 시간과 노력을 절약하기 위해서입니다. 특정 문자열을 찾아서 일일이 대체하는 것은 매우 시간 소모적이며 실수를 범할 수도 있습니다. 반면에 Ruby 프로그래밍 언어를 사용하면 같은 작업을 몇 줄의 코드로 쉽게 처리할 수 있습니다.

## 하는 법

```Ruby
# 검색 대상 텍스트
text = "안녕하세요, 저는 Ruby를 사랑하는 개발자입니다."

# 'Ruby'를 'Python'으로 대체
replaced_text = text.gsub("Ruby", "Python")

# 결과 출력
puts replaced_text

# 출력: 안녕하세요, 저는 Python을 사랑하는 개발자입니다.
```

위 예시 코드에서 `gsub` 메소드를 사용하여 `text` 변수 내부의 "Ruby"를 "Python"으로 대체하였습니다. 이를 통해 간단한 1줄의 코드로 텍스트를 대체하는 작업이 가능합니다. 그리고 대체된 결과를 `puts` 메소드를 사용하여 출력합니다.

## 딥 다이브

Ruby에서는 `String` 클래스의 `gsub` 메소드뿐만 아니라 `sub`, `replace`, `sub!`, `replace!` 등 여러 가지 메소드를 제공하며, 각각의 기능이 조금씩 다릅니다. 예를 들어, `replace` 메소드는 대체된 결과를 반환하는 것이 아니라 기존의 문자열을 수정하며, `sub` 메소드는 첫 번째 검색 대상 문자열만을 대체하는 것입니다.

또한 정규표현식을 이용하여 더 복잡한 검색과 대체 작업을 수행할 수도 있습니다. 정규표현식은 문자열 패턴을 표현하는 방법으로, 조금 더 응용적인 검색 대상을 찾을 수 있도록 도와줍니다.

## 참고

[정규표현식 레퍼런스](https://www.rubyguides.com/2015/06/ruby-regular-expressions/)

[문자열 검색, 대체 관련 메소드 레퍼런스](https://ruby-doc.org/core-2.6/String.html#method-i-gsub)

[Ruby String 클래스 레퍼런스](https://ruby-doc.org/core-2.6/String.html)

[Ruby 정규표현식 딥 다이브 강의](https://www.youtube.com/watch?v=EsaYB0uolX8)