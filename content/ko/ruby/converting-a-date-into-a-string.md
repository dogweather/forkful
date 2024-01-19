---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
날짜를 문자열로 변환하는 것은 일정한 규칙에 따라 날짜 정보를 텍스트 표현으로 바꾸는 것입니다. 이는 사용자가 이해하기 쉬운 형식으로 날짜를 표시하거나, 파일 이름이나 로그 메시지에서 날짜 데이터를 사용할 때 일반적으로 필요합니다.

## 어떻게 사용하나?
Ruby에서 DateTime 객체를 문자열로 변환하는 가장 기본적인 방법은 아래 코드를 참조하십시오.

```Ruby
require 'date'

now = DateTime.now
puts now.to_s
```
이 코드를 실행하면 DateTime 객체의 내용이 문자열로 출력됩니다.

## 깊게 알아보기
Ruby에서 날짜를 문자열로 변환하는 방법은 존재합니다. 우리는 다음과 같이 매우 구체적인 형식의 문자열로 날짜를 변환할 수 있습니다.

```Ruby
require 'date'

now = DateTime.now
puts now.strftime("%Y-%m-%d %H:%M:%S")
```

이 코드를 사용하면 출력되는 문자열은 `"2022-03-25 17:26:15"`와 같은 형식을 가지게 됩니다.

## 참고 자료
* Ruby 공식 문서 [DateTime](https://ruby-doc.org/stdlib-2.7.3/libdoc/date/rdoc/DateTime.html)
* Ruby 공식 문서 [strftime](https://ruby-doc.org/core-2.7.3/Time.html#method-i-strftime)