---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"

category:             "Ruby"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
정규 표현식은 문자열을 처리할 때 패턴을 인식하고 조작하기 위한 강력한 도구다. 프로그래머들은 복잡한 문자열 검색, 대체, 유효성 검사 등을 간단하게 처리하기 위해 정규 표현식을 사용한다.

## How to: (사용법)
```Ruby
# 문자열에서 숫자 찾기
text = "Order number is 12345."
match = text.match(/\d+/)
puts match[0]  # 출력: 12345

# 이메일 형식 검증
email = "user@example.com"
puts email.match(/\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i) ? "Valid" : "Invalid"  # 출력: Valid

# 문자열 치환
str = "2023년 Ruby 3.1.0 버전"
new_str = str.sub(/3\.1\.0/, '3.2.0')
puts new_str  # 출력: 2023년 Ruby 3.2.0 버전
```

## Deep Dive (심화 학습)
정규 표현식은 1950년대에 스티븐 클리니가 수학적 이론에서 착안하여 개발했다. 대안으로 문자열 처리 내장 메소드 사용이 있지만, 정규 표현식은 그 유연성과 강력함에서 경쟁할 수 없다. Ruby는 Oniguruma 라이브러리를 내장하여 정규 표현식 기능을 제공하고, 이는 다양한 패턴 매칭과 효율적인 처리를 가능하게 한다.

## See Also (참고 자료)
- [Ruby 정규 표현식 문서](https://ruby-doc.org/core-3.1.0/Regexp.html)
- [정규 표현식 학습 툴](https://rubular.com/)
- [Oniguruma 깃허브 페이지](https://github.com/kkos/oniguruma)
